<!DOCTYPE html>
<html>
  <head>
    %%(head)%%
  </head>
  <body>
    <div class="page_container">
      %%(top "Index")%%

      <h1>Welcome to PD2AF Converter website!</h1>

      <h2>About</h2>

      <p>PD2AF Converter is a tool for translation of SBGN diagrams from PD to AF form.</p>
      <p><a href="http://sbgn.github.io/sbgn">SBGN (Systems Biology Graphical Notation)</a> is a way we can describe processes inside and outside human and other organism <a href="https://en.wikipedia.org/wiki/Cell_(biology)">cells</a>.</p>
      <p>SBGN Process Decription diagrams <em>(shortly PD)</em> are used to describe biochemical processes, and tend to become enourmously large, when new and new cell internal mechanisms are discovered. For simplifying possibly complex picture, another type of SBGN diagrams may be handy - Activity Flow diagrams <em>(shortly AF).</em></p>
      <p>Usually, information in widespread pathways databases like <a href="http://genome.jp/kegg">KEGG</a>, <a href="http://reactome.org">Reactome</a> etc., is collected in the forms similar to SBGN PD diagrams. Often, they are large and it takes time to understand even the simple features of a processes and described system. Our tool, PD2AF Converter is intended to facilitate complex diagrams study by conversion of PD diagrams into a more simple view of AF diagrams.</p>
      <p>To try PD2AF Converter in work, go to <a href="translator.html">Converter page</a>, provide it with your SBGN PD diagram files and get correspondent SBGN AF diagrams, that you will be able to open in your favorite SBGN editor</p>
      <p>We hope that PD2AF Converter will be useful for you and will help you in your System Biology research activities.</p>


      <h2>Implementation</h2>
      <img src="img/application_components.png" class="illustration" alt="application components" />

      <div class="p">For convenient implementation of Converter function, we have developed some infrastructure around it. The resulted system is shown on the picture above. The modules of the system are:
        <ul>
          [{_web.library}:<li><a href="{url}">{name,id:(~> namefy get-not-empty)}</a> - {description:textify}</li>]
        </ul>
      </div>

      <h2>Converter design</h2>
      <img src="img/translator_design.png" class="illustration" alt="translator design" />

      <div class="p">To convert PD SBGN diagram to the AF SBGN, data undergoes several stages of transformations. Red arrows show implemented transformations.
        <ol>
          <li>We build internal represenattion of the diagram, transforming XML to sexp expression.</li>
          <li>Then, we clean and normalize sexp expression, make it ready for pattern matching</li>
          <li>The stage of conversion of PD to AF itself. Program seeks matches in the patterns and substitute these patterns with prescripted AF forms.</li>
          <li>After this the form is cleaned again, to provide more compact and sensible AF output</li>
          <li>On the last stage we translate sexp expression back to XML, this time SBGN AF ML format. Further this XML file with AF diagram can be uploaded and viewed/edited in the SBGN editor, such as <a href="http://newteditor.org">Newt</a> or <a href="http://sbgn-ed.org">VANTED</a>.</li>
        </ol>
      </div>

      %%(footer)%%
      %%(yandex-counter)%%
    </div>
  </body>
</html>
