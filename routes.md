| Done | Route                                               | Feature         |
| ---- | --------------------------------------------------- | --------------- |
| [ ]  | /package/:package.:format (html)                    | Html            |
| [ ]  | /package/:package.rss                               | PackageFeed     |
| [ ]  | /package/:package/reverse.:format (html)            | Html            |
| [ ]  | /package/:package/reverse/old.:format (html)        | Html            |
| [ ]  | /package/:package/reverse/verbose.:format (html)    | Html            |
| [.]  | /package/:package/revision/:revision.:format        | Core            |
| [.]  | /package/:package/revision/:revision.:format (json) | PackageInfoJSON |
| [/]  | /package/:package/preferred.:format (html)          | Html            |
| ☑️  | /package/:package-version.:format (json)            | PackageInfoJSON |
| ☑️  | /package/:package/:cabal.cabal                      | Core            |
| ☑️  | /package/:package/:tarball.tar.gz                   | Core            |
| ☑️  | /package/:package/changelog.:format (html)          | PackageContents |
| ☑️  | /package/:package/changelog.:format (txt)           | PackageContents |
| ☑️  | /package/:package/dependencies (html)               | Html            |
| ☑️  | /package/:package/deprecated.:format (html)         | Html            |
| ☑️  | /package/:package/distro-monitor.:format (html)     | Html            |
| ☑️  | /package/:package/docs.:format (tar)                | Documentation   |
| ☑️  | /package/:package/docs/..                           | Documentation   |
| ☑️  | /package/:package/readme.:format (html)             | PackageContents |
| ☑️  | /package/:package/readme.:format (txt)              | PackageContents |
| ☑️  | /package/:package/revisions/.:format                | Core            |
| ☑️  | /package/:package/revisions/.:format (html)         | Html            |
| ☑️  | /package/:package/src/..                            | PackageContents |
| ☑️  | /package/:package/upload-time                       | Mirror          |
| ☑️  | /package/:package/uploader                          | Mirror          |
| ☑️  | /package/:packagename.:format (json)                | PackageInfoJSON |
| ☑️  | /packages/deprecated.:format (html)                 | Html            |
<!--
|      | /package/:packagename/revision/:anything.json       | ????             |
|      | /package/:packageid/revision/.json                  | NOT THE SAME AS: |
|      | /package/:packageid/revision/0.json                 | ????             |
-->
