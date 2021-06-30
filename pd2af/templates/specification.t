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
      %%(top "Specification")%%

			<h1>Specification of translation rules</h1>

			<p>Here is a specification for the translation rules. We start from most elementary patterns and follow to more and more complex ones. All these patterns are also test cases for regression testing of the Converter program. With each case we ensure that converter code translates left-hand diagram exactly to the right-hand diagram, while both considering as standalone schemes.</p>

      %%(define baseurl "http://188.166.159.222/specification")%%

      %%(go-through-tests "../tests/tests.pdafer")%%

      %%(expandable (scheme elementary_regulation) (n 1))%%
      %%(expandable (scheme source_and_sinks) (n 2))%%
      %%(expandable (scheme oligomerization) (n 3))%%
      %%(expandable (scheme translocation) (n 4))%%
      %%(expandable (scheme complexes) (n 5))%%
      %%(expandable (scheme hidden_inhibition) (n 6))%%
      %%(expandable (scheme patterns) (n 7))%%
      %%(expandable (scheme simple_schemes) (n 8))%%
      %%(expandable (scheme new_schemes) (n 9))%%
      %%(expandable (scheme metabolism_regulation_maps) (n 10))%%

      <div class="placeholder" ng-hide="show1 || show2"></div>

      %%(footer)%%
      %%(yandex-counter)%%
    </div>
  </body>
</html>
