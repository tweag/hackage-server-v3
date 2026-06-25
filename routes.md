| Done | Route                                               | Feature         | Method   |
| ---- | --------------------------------------------------- | --------------- | -------- |
| [ ]  | /package/:package.:format (html)                    | Html            | GET      |
| [x]  | /package/:packagename.:format (json)                | PackageInfoJSON | GET      |
| [x]  | /package/:package-version.:format (json)            | PackageInfoJSON | GET      |
| [ ]  | /package/:package.rss                               | PackageFeed     | GET      |
| [x]  | /package/:package/:cabal.cabal                      | Core            | GET      |
| [ ]  | /package/:package/:tarball.tar.gz                   | Core            | GET      |
| [ ]  | /package/:package/changelog.:format (html)          | PackageContents | GET      |
| [ ]  | /package/:package/changelog.:format (txt)           | PackageContents | GET      |
| [ ]  | /package/:package/dependencies (html)               | Html            | GET      |
| [ ]  | /package/:package/deprecated.:format (html)         | Html            | PUT      |
| [ ]  | /package/:package/distro-monitor.:format (html)     | Html            | GET      |
| [ ]  | /package/:package/docs.:format (tar)                | Documentation   | GET      |
| [ ]  | /package/:package/docs/..                           | Documentation   | GET      |
| [/]  | /package/:package/preferred.:format (html)          | Html            | GET      |
| [ ]  | /package/:package/readme.:format (html)             | PackageContents | GET      |
| [ ]  | /package/:package/readme.:format (txt)              | PackageContents | GET      |
| [ ]  | /package/:package/reports/.:format (txt)            | BuildReports    | GET      |
| [ ]  | /package/:package/reverse.:format (html)            | Html            | GET      |
| [ ]  | /package/:package/reverse/old.:format (html)        | Html            | GET      |
| [ ]  | /package/:package/reverse/verbose.:format (html)    | Html            | GET      |
| [ ]  | /package/:package/revision/:revision.:format        | Core            | GET      |
| [ ]  | /package/:package/revision/:revision.:format (json) | PackageInfoJSON | GET      |
| [ ]  | /package/:package/revisions/.:format                | Core            | GET      |
| [ ]  | /package/:package/revisions/.:format (html)         | Html            | GET      |
| [ ]  | /package/:package/src/..                            | PackageContents | GET      |
| [ ]  | /package/:package/upload-time                       | Mirror          | GET      |
| [ ]  | /package/:package/uploader                          | Mirror          | GET      |
| [ ]  | /packages/.:format (html)                           | Html            | GET      |
| [ ]  | /packages/.:format (json)                           | Core            | GET      |
| [ ]  | /packages/reverse.:format (html)                    | Html            | GET      |
