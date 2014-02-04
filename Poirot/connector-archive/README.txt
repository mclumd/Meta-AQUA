Using the Poirot LISP connector
--------------------------------

LISP implementation required: Allegro V 8.0
  This connector could work on earlier versions of Allegro, but is tested only
  with V8.0

SETUP
-----

1)  Allegro JLinker Setup

    Either
    a1) edit the copy of jl-config.cl present in the lisp/src/lisp/ directory, or
    a2) copy allegro/jlinker/jl-config.cl to the lisp/src/lisp/ directory.

    b) copy jlinker/jlinker.jar to the allegro main directory, which is one
       level up from the jlinker subdirectory.

    c) customize jl-config.cl as described in the comments of that file.

2)  Poirot LISP Connector setup

    a) examine comments at the top of the loader.cl file

    b) create a .clinit.cl initialization file, or modify an existing one, as
       described by the comments

    a) edit value of the following variables 

       Modify the value of the *poirot-java-home* variable to match your poirot
       root directory.

       Modify the value of the *poirot-port* variable if you'd like the Lisp to
       Java connection to happen on a different port

       The *poirot-component-namespace* variable will be used as the default
       namespace for any OWL objects you publish to the blackboard.  Modify the
       value as necessary
       
       Change the *poirot-component-name* and *poirot-component-description* as
       needed.


3)  Poirot Configuration Setup

    a) In your poirot-config.xml, located in the poirot subdirectory of the
       tomcat directory on the machine running the Poirot Core, add an entry
       for this component:

  <component class="com.bbn.poirot.testlisp.RemoteLispComponent">
    <connector type="remote" host="localhost" port="9090" />
  </component>
    


USING THE CONNECTOR
--------------------

1) Instruction from loader.cl:

 1.  Start Allegro Common Lisp 
 2.  (start-poirot-connection *poirot-sample-query*)
 3.  Start Proxy code
     C:\poirot\coretrunk\proxy\runProxyServer.bat
 4.  Start Tomcat at default
     "C:\Program Files\Apache Software Foundation\Tomcat 5.5\bin\tomcat5.exe"
 5.  Open browser at default http://localhost:8008/poirot-servlet/
 6.  Select Debugger link, Click Run button
 7.  Perform actions with POIROT interface
 8.  Select Session Manager link, click 2bb button
 9.  POIROT meta-controller should make available Semantic Trace 
 10. Process trace, post hypotheses
 11. (javatools.jlinker:jlinker-end)
 12. CTRL-c in TomCat window
 13. CTRL-c in Proxy window


2) Construct a list of subscription queries for your component.

   An example query is given by the string *poirot-sample-query*

3) To start the java connection, call (start-poirot-connection query-list)
   [See 1)2. above]
   Where query-list is a list of string queries, or one single query string.

   This creates the Java JVM and hooks it to jlinker your allegro lisp session.

;;ji[6]: Started Lisp server at localhost:1695.
;;ji[6]: Advertising Lisp server at localhost:1695.
;;ji[6]: Starting Java VM ...
;;ji[6]: Expecting announceServer message from Java.
;;ji[6]: Java server is ready at localhost:1697
;;ji[6]: Lisp client connected to Java server localhost:1697
;;ji[6]: Expecting connectToServer nessage from Java.
;;ji[6]: Connected Java client to Lisp server.
T

A failure here is probably due to an improper jlinker installation, or an
incorrect classpath.  Double check that the steps in SETUP above were followed
correctly and check the classpath console output (make sure the file pointers
are correct).

4) Start the Poirot Core, see the README at poirot\build. See also 1) above.

5) After the Poirot meta controller has started your component, you can query
   its status:

CG-USER(6): (get-component-name)
"TestLispHypothesizer"
CG-USER(7): (get-component-state)
"Loaded"

6) Execute Method

   When the queries you have subscribed to have data available, the poirot meta
   controller will eventually call the execute method of the java portion of
   your component.  This java method will interact with your lisp envrionment
   in one of two ways:

   a) Callback method.  

   The execute callback should take one single argument, which will be the
   subscription result list.  This list will be a list of lists of strings,
   which represent the data values you subscribed to.

   Set the variable *execute-callback* to the symbol name of the method you'd
   like to have executed.

   File loader.cl has a comment at the beginning that provides an example that
   can be run to demonstrate this proicedure. The code in the comment should be
   placed in your ACL initialization file, .clinit.cl, so that it is run when
   you start Allegro. The callback is as follows.

    (setf *execute-callback* 
      #'(lambda (results)
	  (test-My-Component results)))

   The code for test-My-Component is in example.cl. The file
   example-callback.txt contains a listing of the output produced with this
   procedure on a simple series of sematic web service calls.

   b) Polling

   The java execute method will call the lisp function pc::poirot-execute, which
   activates the above callback and sets the variable *new-subscription-data*

   You can have a lisp process poll for a non-null value for
   *new-subscription-data* using the function
   get-new-subscription-results. File example.cl provides an illustration of
   this procedure. To run the poling example, you should not assign a value to
   the variable *execute-callback* as discussed in procedure a) above.

   The file example-poling.txt contains a listing of the output produced with
   this procedure on a simple sematic web service call.
   
7) Finishing the execute

   The poirot meta controller waits for your lisp process to finish before
   continuing to let other components run.  You can tell the meta controller
   that you're finished by calling the function done-processing.

   If you have other blackboard queries or publishes to execute, you must do
   those before calling done-processing.

