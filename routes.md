| Done | Route                                               | Feature         |
| ---- | --------------------------------------------------- | --------------- |
| [ ]  | /package/:package.:format (html)                    | Html            |
| [ ]  | /package/:package.rss                               | PackageFeed     |
| [ ]  | /package/:package/changelog.:format (html)          | PackageContents |
| [x]  | /package/:package/changelog.:format (txt)           | PackageContents |
| [ ]  | /package/:package/docs.:format (tar)                | Documentation   |
| [x]  | /package/:package/readme.:format (html)             | PackageContents |
| [x]  | /package/:package/readme.:format (txt)              | PackageContents |
| [ ]  | /package/:package/reverse.:format (html)            | Html            |
| [ ]  | /package/:package/reverse/old.:format (html)        | Html            |
| [ ]  | /package/:package/reverse/verbose.:format (html)    | Html            |
| [.]  | /package/:package/revision/:revision.:format        | Core            |
| [.]  | /package/:package/revision/:revision.:format (json) | PackageInfoJSON |
#        /package/:packagename/revision/:anything.json       ????
#        /package/:packageid/revision/.json                  NOT THE SAME AS:
#        /package/:packageid/revision/0.json                 ????
| [/]  | /package/:package/preferred.:format (html)          | Html            |
| [x]  | /package/:package-version.:format (json)            | PackageInfoJSON |
| [x]  | /package/:package/:cabal.cabal                      | Core            |
| [x]  | /package/:package/:tarball.tar.gz                   | Core            |
| [x]  | /package/:package/dependencies (html)               | Html            |
| [x]  | /package/:package/deprecated.:format (html)         | Html            |
| [x]  | /package/:package/distro-monitor.:format (html)     | Html            |
| [x]  | /package/:package/docs/..                           | Documentation   |
| [x]  | /package/:package/revisions/.:format                | Core            |
| [x]  | /package/:package/revisions/.:format (html)         | Html            |
| [x]  | /package/:package/src/..                            | PackageContents |
| [x]  | /package/:package/upload-time                       | Mirror          |
| [x]  | /package/:package/uploader                          | Mirror          |
| [x]  | /package/:packagename.:format (json)                | PackageInfoJSON |
| [x]  | /packages/deprecated.:format (html)                 | Html            |

