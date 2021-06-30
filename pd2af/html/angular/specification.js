
angular
  .module('specification',
          [],
          function($interpolateProvider) {
            $interpolateProvider.startSymbol('#(');
            $interpolateProvider.endSymbol(')#')
          })
  .controller("specification_controller", function($scope) {
    var N = 30;
    $scope.show = Array(N).fill(false);
    $scope.bugs = Array(N).fill(false);

    $scope.passed = Array(N).fill(0);
    $scope.failed = Array(N).fill(0);

    $scope.toggle_show = function(n) {
      $scope.show[n] = !$scope.show[n];
    }

    $scope.set_bugs_flag = function(n) {
      $scope.bugs[n] = true;
    }

    $scope.increment_passed = function(n) {
      $scope.passed[n]++;
    }

    $scope.increment_failed = function(n) {
      $scope.failed[n]++;
    }

  });
