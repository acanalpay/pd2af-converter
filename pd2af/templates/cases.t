<!DOCTYPE html>
<html lang="ru">
  <head>
    %%(head)%%
    <script src="https://ajax.googleapis.com/ajax/libs/angularjs/1.6.4/angular.min.js"></script>
    <script src="angular/specification.js"></script>
    <link rel="stylesheet" href="styles/specification.css">
  </head>
  <body ng-app="specification" ng-controller="specification_controller">
    <div class="page_container">
      %%(top "Cases")%%

			<h1>Cases of translation rules</h1>

      %%(define baseurl "http://188.166.159.222/specification")%%

      %%(go-through-tests "../tests/tests_cases.pdafer")%%

      %%(expandable-cases (scheme cases) (n 1))%%

      <div class="placeholder" ng-hide="show1 || show2"></div>

      %%(footer)%%
      %%(yandex-counter)%%
    </div>
  </body>
</html>
