{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Markdown
  ( renderMarkdown
  , renderMarkdownRel
  , supposedToBeMarkdown
  ) where

import Data.Coerce
import Data.Char (toLower)
import Commonmark
import Commonmark.Extensions
import Commonmark.Extensions.Footnote()
import Commonmark.Extensions.Math()
import Control.Monad.Identity (runIdentity)
import Data.ByteString.Lazy qualified as BS (ByteString, toStrict)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.Encoding.Error qualified as T (lenientDecode)
import Data.Text.Lazy qualified as TL
import Data.Typeable (Typeable)
import Network.URI (isRelativeReference)
import System.FilePath.Posix  (takeExtension)
import Text.HTML.SanitizeXSS as XSS


-- RelHtml wraps Html, and mostly behaves the same, except that
-- relative links in images and urls have "src/" prepended.
newtype RelHtml a = RelHtml { unRelHtml :: Html a }
  deriving newtype (Show, Semigroup, Monoid, HasAttributes, ToPlainText, HasEmoji, HasStrikethrough, HasMath)

instance Rangeable (Html a) => Rangeable (RelHtml a) where
  ranged sr (RelHtml x) = RelHtml $ ranged sr x

instance (Rangeable (Html a), Rangeable (RelHtml a)) => IsInline (RelHtml a) where
  lineBreak   = coerce $ lineBreak   @(Html a)
  softBreak   = coerce $ softBreak   @(Html a)
  str         = coerce $ str         @(Html a)
  entity      = coerce $ entity      @(Html a)
  escapedChar = coerce $ escapedChar @(Html a)
  emph        = coerce $ emph        @(Html a)
  strong      = coerce $ strong      @(Html a)
  code        = coerce $ code        @(Html a)
  rawInline   = coerce $ rawInline   @(Html a)
  link target title = RelHtml . link (adjustRelativeLink target) title . unRelHtml
  image target title = RelHtml . image (adjustRelativeLink target) title . unRelHtml

instance (Rangeable (Html a), IsInline (RelHtml a))
     => IsBlock (RelHtml a) (RelHtml a) where
  paragraph               = coerce $ paragraph               @(Html a) @(Html a)
  plain                   = coerce $ plain                   @(Html a) @(Html a)
  thematicBreak           = coerce $ thematicBreak           @(Html a) @(Html a)
  blockQuote              = coerce $ blockQuote              @(Html a) @(Html a)
  codeBlock               = coerce $ codeBlock               @(Html a) @(Html a)
  heading                 = coerce $ heading                 @(Html a) @(Html a)
  rawBlock                = coerce $ rawBlock                @(Html a) @(Html a)
  referenceLinkDefinition = coerce $ referenceLinkDefinition @(Html a) @(Html a)
  list                    = coerce $ list                    @(Html a) @(Html a)

instance HasPipeTable (RelHtml a) (RelHtml a) where
  pipeTable = coerce $ pipeTable @(Html a) @(Html a)

instance (Rangeable (Html a), Rangeable (RelHtml a))
         => HasTaskList (RelHtml a) (RelHtml a) where
  taskList = coerce $ taskList @(Html a) @(Html a)

instance Rangeable (Html a) => HasFootnote (RelHtml a) (RelHtml a) where
  footnote     = coerce $ footnote     @(Html a) @(Html a)
  footnoteList = coerce $ footnoteList @(Html a) @(Html a)
  footnoteRef  = coerce $ footnoteRef  @(Html a) @(Html a)

-- | Prefix relative links with @src/@.
adjustRelativeLink :: T.Text -> T.Text
adjustRelativeLink url
  | isRelativeReference (T.unpack url) &&
    not ("/" `T.isPrefixOf` url)
              = "src/" <> url
  | otherwise = url

-- | Render markdown to HTML.
--
-- >>> renderMarkdown "test" "Please send bug reports to hackage-server@gmail.com."
-- <p>Please send bug reports to <a href="mailto:hackage-server@gmail.com">hackage-server@gmail.com</a>.</p>
-- <BLANKLINE>
--
-- >>> renderMarkdown "test" "Published to http://hackage.haskell.org/foo3/bar."
-- <p>Published to <a href="http://hackage.haskell.org/foo3/bar">http://hackage.haskell.org/foo3/bar</a>.</p>
-- <BLANKLINE>
--
-- >>> renderMarkdown "test" "Issue #1105:\n- pipes\n- like `a|b`\n- should be allowed in lists"
-- <p>Issue #1105:</p>
-- <ul>
-- <li>pipes
-- </li>
-- <li>like <code>a|b</code>
-- </li>
-- <li>should be allowed in lists
-- </li>
-- </ul>
-- <BLANKLINE>
--
-- >>> renderMarkdown "test" "Tables should be supported:\n\nfoo|bar\n---|---\n"
-- <p>Tables should be supported:</p>
-- <table>
-- <thead>
-- <tr>
-- <th>foo</th>
-- <th>bar</th>
-- </tr>
-- </thead>
-- </table>
-- <BLANKLINE>
--
renderMarkdown
  :: String         -- ^ Name or path of input.
  -> BS.ByteString  -- ^ Commonmark text input.
  -> T.Text     -- ^ Rendered HTML.
renderMarkdown = renderMarkdown' (renderHtml :: Html () -> TL.Text)

-- | Render markdown to HTML, prefixing relative links with @src/@.
--
-- >>> renderMarkdownRel "test" "See [world file](world.txt)."
-- <p>See <a href="src/world.txt">world file</a>.</p>
-- <BLANKLINE>
--
renderMarkdownRel
  :: String         -- ^ Name or path of input.
  -> BS.ByteString  -- ^ Commonmark text input.
  -> T.Text     -- ^ Rendered HTML.
renderMarkdownRel = renderMarkdown' (renderHtml . unRelHtml :: RelHtml () -> TL.Text)

-- | Prerequisites for 'commonmarkWith' with 'gfmExtensions' and 'mathSpec'.
type MarkdownRenderable a =
  ( Typeable a
  , HasEmoji a
  , HasFootnote a a
  , HasMath a
  , HasPipeTable a a
  , HasStrikethrough a
  , HasTaskList a a
  , IsBlock a a
  , IsInline a
  , ToPlainText a
  )

-- | Generic gfm markdown rendering.
renderMarkdown'
  :: MarkdownRenderable a
  => (a -> TL.Text)  -- ^ HTML rendering function.
  -> String          -- ^ Name or path of input.
  -> BS.ByteString   -- ^ Commonmark text input.
  -> T.Text          -- ^ Rendered HTML.
renderMarkdown' render name md =
  either (const txt) mdToHTML $
    runIdentity $ commonmarkWith spec name txt
  where
  -- Input
  txt = T.decodeUtf8With T.lenientDecode . BS.toStrict $ md
  -- Fall back to HTML if there is a parse error for markdown
  -- Conversion of parsed md to HTML
  mdToHTML = sanitizeBalance . TL.toStrict . render
  -- Specification of the markdown parser.
  -- Andreas Abel, 2022-07-21, issue #1105.
  -- Workaround for https://github.com/jgm/commonmark-hs/issues/95:
  -- Put the table parser last.
  spec = mconcat $
    mathSpec :
    -- all the gfm extensions except for tables
    emojiSpec :
    strikethroughSpec :
    autolinkSpec :
    autoIdentifiersSpec :
    taskListSpec :
    footnoteSpec :
    -- the default syntax
    defaultSyntaxSpec :
    -- the problematic table parser
    pipeTableSpec :
    []

-- | Does the file extension suggest that the file is in markdown syntax?
supposedToBeMarkdown :: FilePath -> Bool
supposedToBeMarkdown fname = fmap toLower (takeExtension fname) `elem` [".md", ".markdown"]
