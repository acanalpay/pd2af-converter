<!DOCTYPE html>
<html lang="ru">
  <head>
    <meta name="viewport" content="width=device-width, initial-scale=1"> <!-- bulma -->
    <script src="https://ajax.googleapis.com/ajax/libs/angularjs/1.6.4/angular.min.js"></script>
    <!-- <link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/bulma/0.6.2/css/bulma.min.css"> -->
    <script defer src="https://use.fontawesome.com/releases/v5.0.0/js/all.js"></script>
    <script src="angular/components/ng-file-upload/ng-file-upload-all.js"></script>
    <script src="angular/translator.js"></script>
    %%(head)%%
    <link rel="stylesheet" href="styles/translator.css">
  </head>
  <body>
    <div class="page_container">
      %%(top "Translator")%%
      <form ng-app="translator" ng-controller="AppController" ng-init="current_file='Select file'" name="form">
        <section class="section">
          <h1>PD > AF</h1>
          <p>Translate SBGN diagrams from Process Description to Activity Flow. Please, upload file in XML format.</p>
          <div class="box">
            <div class="buttons">
                <a ng-class="['button', translate_button? 'selected' : 'action']" ngf-select="refresh_filename($file)" ng-model="file" name="file">[[file_selector_button_text]]</a>
                <a class="button action ng-hide" ng-show="translate_button" ng-click="submit()">Convert</a>
            </div>
          </div>
          <div ng-show="error" class="box ng-hide error">
            <h2>Translation error</h2>
            <span>[[error_message]]</span>
            <div class="box"><a class="button reset" ng-click="reset()">Reset</a></div>
          </div>
          <!-- <div ngshow="calculation_img"><img src="img/loading.gif" /></div> -->
          <div ng-show="download_button" class="box ng-hide">
            <table class="results_table">
              <tbody>
                <tr>
                  <td><p>Open result as AF SBGN file:</p></td>
                  <td><a href="/generated/[[af_filename]]">[[af_filename]]</a></td>
                </tr>
                <tr>
                  <td><p>Open result in Newt Editor:</p></td>
                  <td><a href="http://web.newteditor.org/?URL=[[af_fileurl]]" target="_blank"><img width="60" src="http://web.newteditor.org/app/img/newt-logo.png" /></a></td>
                </tr>
                <tr><td class="c" colspan="2"><div><a class="button reset" ng-click="reset()">Reset</a></div></td></tr>
              </tbody>
            </table>
          </div>
          <div class="placeholder"></div>
        </section>
      </form>
      %%(footer)%%
      %%(yandex-counter)%%
    </div>
  </body>
</html>
