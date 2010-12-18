Raconteur: What is it?
======================

Raconteur is a web framework and platform for R. The goals are to:

	* Provide an easy way for statisticians to tell a story with
	  their data and models. This approach will 

	* Allow creation of web services style applications with the possibility 
      of chaining them together in a data pipeline.

Raconteur is installed as an R package and deployed on an Apache
web server.  Raconteur applications (or apps for short) are created
on users' local machines and then uploaded to the server. There is a
Raconteur root URL (ROOT_URL), and every app is located on the root url
at http://ROOT_URL/app.

Setup
=====

This is a first pass at dispatching raconteur apps. Change the paths
appropriately and add the following to apache:

<Location /raconteur>
		REvalOnStartup "options('raconteur.app_path'='/usr/lib/R/site-library/raconteur/inst')"
		REvalOnStartup "options('raconteur.admin_app'='/usr/lib/R/site-library/raconteur/inst/admin')"
		REvalOnStartup "options('raconteur.root_url'='/raconteur')"
		REvalOnStartup "options(brew.chdir=TRUE)"
        SetHandler r-handler
		RHandler raconteur::rApache_handler
</Location>
<Location /raconteur/admin>
        SetHandler r-handler
		RHandler raconteur::rApache_admin_handler
</Location>

	REvalOnStartup "RACONTEUR_ROOT='/path_to_raconteur_apps'"
	<Location /raconteur>
		SetHandler r-handler
		RPreserveEnv
		RFileHandler /path_to_raconteur/rapache_dispatcher.R::dispatch
	</Location>


Application Skeleton Examples
-----------------------------

  # Short Example
  p <- function(start, finish){
    start:finish
  }

  app.skeleton(p)

  # Full Example
  app.skeleton(app.skeleton)
