

angular
  .module('translator',
          ['ngFileUpload'],
          function($interpolateProvider) {
            $interpolateProvider.startSymbol('[[');
            $interpolateProvider.endSymbol(']]')
          })
  .controller("AppController", ['$scope', 'Upload', function($scope, Upload) {

    $scope.delayed = {};
    $scope.download_button = false;
    $scope.file_selector_button_text = "Select file";
    // $scope.calculation_img = false;
    console.log("AppController");
    $scope.refresh_filename = function(file) {
      $scope.translate_button = true;
      $scope.file_selector_button_text = file.name;
      $scope.addClass("selected");
    }

    $scope.submit = function() {
      console.log("Submit");

      var pd_file = $scope.form.file;
      $scope.filename = $scope.file.name;
      if ($scope.file) {
        $scope.upload($scope.file);
      }
    }

    $scope.upload = function(file) {
      console.log(typeof file);
      Upload.upload({
          // url: 'http://188.166.159.222/translate',
          url: '/translate',
          data: {'file': file, 'action': 'parse', 'filename': $scope.filename}
      }).then(function(response) {
          // $scope.calculation_img = false;
          var result = response.data;
          if (result.error_message) {
            $scope.error = true;
            $scope.error_message = response.data.error_message;
          } else {
            $scope.af_filename = result.af_filename;
            $scope.af_fileurl = result.af_fileurl;
            if ($scope.af_filename) {
              $scope.download_button = true;
            }
            else {
              $scope.download_button = false;
            }
          }
      }, function (response) {
          console.log('Error status: ' + response);
      }, function (evt) {
            // $scope.calculation_img = true;
            var progressPercentage = parseInt(100.0 * evt.loaded / evt.total);
            // console.log('progress: ' + progressPercentage + '% ' + evt.config.data.file.name);
      });
    }

    $scope.reset = function() {
      $scope.download_button = false;
      $scope.translate_button = false;
      $scope.error = false;
      $scope.file_selector_button_text = "Select file";
      var el = angular.element(document.querySelector("#file_selector"));
      el.addClass("action");
      // $scope.ResultPD = "";
      // $scope.ResultAF = "";
      // $scope.ResultAF_XML = "";
    }
  }])
