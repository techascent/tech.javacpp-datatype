(defproject techascent/tech.javacpp-datatype "0.2.0"
  :description "Bindings between javacpp and tech.datatype"
  :url "http://github.com/tech-ascent/tech.javacpp-datatype"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [techascent/tech.datatype "0.3.21"]
                 [org.bytedeco/javacpp "1.4"]
                 [thinktopic/think.resource "1.2.1"]]

  :profiles {:dev
             ;; there are a set of small functions that aren't compiled into the javacpp library but into each
             ;; presets library.  So in order to test or do development we have to load one of the presets librarys;
             ;; any one that uses javacpp will do
             {:dependencies [[org.bytedeco.javacpp-presets/opencv-platform "3.4.0-1.4"]]}})
