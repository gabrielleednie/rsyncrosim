# Copyright (c) 2021 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# MIT License ## lines 41, 175, 226 fail in test env. 

old_dir <- getwd()
temp_dir <- tempdir()
dir.create(temp_dir)
setwd(temp_dir)

mySsim <- session()
addPackage(session = mySsim, name = "stsimsf")
addPackage(session = mySsim, name = "helloworld")

test_that("Tests of Session - assumes SyncroSim is installed", {
  skip_on_cran()
  expect_is(mySsim, "Session")
  expect_equal(file.exists(filepath(mySsim)), TRUE) # Lists the folder location of SyncroSim session
  expect_output(str(version(mySsim)), "chr", fixed = TRUE) # Lists the version of SyncroSim session
  expect_equal(names(package(mySsim)), c("name", "description", "version")) # Dataframe of the modules installed with this verions of SyncroSim.
  expect_equal(names(package(mySsim)), c("name", "description", "version")) # Dataframe of the modules installed with this verions of SyncroSim.
  expect_equal(names(package(mySsim)), c("name", "description", "version")) # Dataframe of the models installed with this version of SyncroSim, listing all of its properties as columns
  expect_equal(names(package(mySsim)), c("name", "description", "version")) # Dataframe of the models installed with this version of SyncroSim, listing all of its properties as columns

  mySession <- session(silent = FALSE) # modify default session settings
  expect_equal(silent(mySession), FALSE)
  silent(mySession) <- TRUE
  expect_equal(silent(mySession), TRUE)
  expect_output(session(printCmd = TRUE), "--version")
})

test_that("Tests of command  - assumes SyncroSim is installed", {
  skip_on_cran()
  expect_equal(command("help", mySsim)[1], "SyncroSim System Console")
  expect_equal(command(c("list", "help"), mySsim)[1], "Lists existing items")
  expect_equal(command("--create --help", mySsim)[1], "Creates an item")
  expect_equal(command(list(create = NULL, help = NULL), mySsim)[1], "Creates an item")

  expect_error({delete(paste0(getwd(), "/temp.ssim"), force = TRUE)}, 
               "Library not found")
  args <- list(create = NULL, library = NULL, name = paste0(getwd(), "/temp.ssim"), package = "nonexistentpackage")
  output <- command(args, mySsim)
  # TODO Fails in test env only
  expect_equal(output[1], "The specified package is not installed.")
})

test_that("Tests of Library - assumes SyncroSim is installed", {
  skip_on_cran()
  myLibrary <- ssimLibrary(name = "temp", session = mySsim) # create new library using default model
  expect_equal(file.exists(filepath(myLibrary)), TRUE)
  expect_equal(as.character(package(myLibrary)$name), "stsim")
  expect_equal(delete(myLibrary, force = TRUE), TRUE)
  expect_equal(file.exists(filepath(myLibrary)), FALSE)

  myLibrary <- ssimLibrary(name = "SSimLibrary", session = mySsim)
  expect_equal(file.exists(filepath(myLibrary)), TRUE)
  expect_equal(name(myLibrary), "SSimLibrary")

  # With addons
  expect_equal(nrow(subset(addon(myLibrary), enabled)), 0)
  allAdds <- addon(myLibrary)
  expect_equal(names(allAdds), c("name", "description", "enabled", "currentVersion", "minimumVersion"))
  expect_equal(names(addon(mySsim)), c("name", "description", "version", "extends"))
  expect_equal(delete(myLibrary, force = TRUE), TRUE)

  allAdds <- subset(allAdds, name != "stsim-cbm") # Requires stsim-stockflow to be added first

  if (nrow(allAdds) > 0) {
    cAdd <- allAdds$name[1]
    # TODO Fails to detect stsimsf is installed => potential bug with console
    myLibrary <- ssimLibrary(name = "NewLibrary", addon = c(cAdd), session = mySsim)
    expect_equal(subset(addon(myLibrary), enabled)$name, cAdd)
    expect_equal(disableAddon(myLibrary, cAdd)[[cAdd]], TRUE)
    expect_equal(nrow(subset(addon(myLibrary), enabled)), 0)
    expect_equal(enableAddon(myLibrary, cAdd)[[cAdd]], TRUE)
    expect_equal(subset(addon(myLibrary), enabled)$name, cAdd)
  }

  # Get/set the various properties of the library
  expect_is("session<-"(myLibrary, mySsim), "SsimLibrary")

  # Changed from NA to FALSE to match updated behaviors
  expect_equal(ssimUpdate(myLibrary), FALSE)
  expect_equal(names(ssimLibrary(myLibrary, session = mySsim)), c("property", "value"))
  expect_equal(class(ssimLibrary(myLibrary, summary = FALSE, mySsim))[1], "SsimLibrary")

  name(myLibrary) <- "Fred"
  expect_equal(name(myLibrary), "Fred")
  expect_equal(backup(myLibrary), TRUE)
  expect_equal(dir.exists(paste0(filepath(myLibrary), ".backup")), TRUE)

  description(myLibrary) <- "A new description.\nTry a linebreak." # NOTE: \n adds a linebreak to the description
  expect_equal(description(myLibrary)[2], "Try a linebreak.")

  owner(myLibrary) <- "Fred"
  expect_equal(owner(myLibrary), "Fred")
  readOnly(myLibrary) <- TRUE
  expect_equal(readOnly(myLibrary), TRUE)
  readOnly(myLibrary) <- FALSE
  expect_equal(readOnly(myLibrary), FALSE)
  expect_equal(grepl("at", dateModified(myLibrary)), TRUE)
  ret <- delete(myLibrary, force = TRUE)
})

test_that("Tests of projects and scenarios - assumes SyncroSim is installed", {
  skip_on_cran()
  
  myLib <- ssimLibrary(name = "temp26", session = mySsim, overwrite = TRUE)
  myOtherLib <- ssimLibrary(name = "temp27", session = mySsim, overwrite = TRUE)
  myOtherLibProj <- project(ssimObject = myOtherLib, project = "MyProj")
  myOtherScn <- scenario(myOtherLibProj, scenario = "other")

  expect_is(myOtherScn, "Scenario")
  expect_equal(scenario(myOtherLib)$ScenarioID, 1)
  ret <- delete(myOtherLib, scenario = "other", force = TRUE)
  expect_equal(nrow(scenario(myOtherLib)), 0)
  myOtherScn <- scenario(myOtherLib, scenario = "other2")

  expect_equal(names(project(myOtherLib)), c("ProjectID", "Name", "Owner",
                                             "DateLastModified", "IsReadOnly"))
  expect_equal(names(scenario(myOtherLib)), c("ScenarioID", "ProjectID", "Name",
                                              "IsResult", "ParentID", "Owner",
                                              "DateLastModified", "IsReadOnly",
                                              "MergeDependencies",
                                              "IgnoreDependencies",
                                              "AutoGenTags"))

  myProject <- project(myLib, project = "temp")
  expect_is(myProject, "Project")
  expect_equal(names(datasheet(myProject)), c("scope", "name", "displayName")) # Only scope, name and displayName returned
  expect_equal(is.element("corestime_Maps",
                          datasheet(myLib, project = "temp",
                                    summary="CORE")$name), TRUE) # same thing, but more system calls. Generally using ids/objects is faster than using names.
  expect_equal(names(datasheet(myProject, optional = TRUE)), c("scope", "package", "name", "displayName", "isSingle", "isOutput"))

  expect_error(scenario(myLib, scenario = 1), "Scenario ids (1) not found in ssimObject. To make new scenarios, please provide names (as one or more character strings) to the scenario argument of the scenario() function. SyncroSim will automatically assign scenario ids.", fixed = TRUE) # Fail: need a name to create a scenario
  myScn <- scenario(myLib, scenario = "one") # Ok because only one project in the library.
  expect_is(myScn, "Scenario")
  myProject <- project(myLib, project = "temp2")
  expect_equal(project(myLib)$Name, c("temp", "temp2"))
  myScn <- scenario(myLib, scenario = "one") # Ok because only one scenario of this name occurs in the library.
  myScn <- scenario(myProject, scenario = "one") # Creates a new scenario called "one" in the second project.
  expect_equal(scenario(myLib)$Name, c("one", "one"))

  expect_error(scenario(myLib, scenario = "one"), "The ssimObject contains more than one scenario called one. Specify a scenario id: 1,2", fixed = TRUE) # Fails because now there are two scenarios called "one" in the library.
  myScn <- scenario(myProject, scenario = "one", overwrite = TRUE) # Overwrites existing scenario, assigns new id.
  expect_equal(scenario(myLib)$ScenarioID, c(1, 3))
  myScn <- scenario(myProject, scenario = "two", overwrite = TRUE, sourceScenario = 1) # Can copy scenarios between projects.
  expect_equal(projectId(myScn), 14)
  myScn <- scenario(myProject, scenario = "other", overwrite = TRUE, sourceScenario = myOtherScn) # Can copy scenarios between libraries if sourceScenario is a scenario object.
  expect_equal(scenarioId(myScn), 4)

  myOtherProject <- project(myOtherLib, project = "copy", sourceProject = myProject) # Can copy projects among libraries provided that sourceProject is a Project object.

  # TODO This fails for an unknown reason => BUG SUBMITED
  myOtherProject <- project(myLib, project = "copy", sourceProject = 11) # Copy a project within the same library.
  expect_equal(projectId(myOtherProject), 21) # 21
  expect_warning(project(myLib, project = "temp", sourceProject = "temp2"), "Project  (1) already exists, so sourceProject argument was ignored.", fixed = TRUE) # Warns that sourceProject is ignored because "temp" already exists.
  myOtherProject <- project(myLib, project = "copy2", sourceProject = "temp2") # Copy a project by name
  expect_equal(project(myLib)$name, c("copy", "copy2", "temp", "temp2"))
  ret <- delete(myProject, scenario = "one", force = TRUE)
  myScn <- scenario(myProject, scenario = "one", sourceScenario = "one") # Ok because only one possible source
  expect_equal(scenarioId(myScn), 6)
  expect_warning(scenario(myProject, scenario = "one", sourceScenario = "one"), "ourceScenario was ignored because scenario already exists.", fixed = TRUE) # Warns that sourceScenario will be ignored.
  expect_error(scenario(myProject, scenario = "three", sourceScenario = "one"), "There is more than one scenario called one in the SsimLibrary. Please provide a sourceScenario id: 1,6", fixed = TRUE) # Fail if more than one scenario named sourceScenario in the library.

  expect_equal(nrow(scenario(myScn, summary = TRUE)), 1) # return summary info
  expect_is(datasheet(myScn, "core_SysFolder"), "data.frame") # returns a datasheet
  expect_is(datasheet(myScn, "core_SysFolder", forceElements = TRUE), "list") # returns a list

  expect_equal(length(datasheet(myScn, c("core_Settings", "core_SysFolder"))), 2) # returns a list

  allScns <- scenario(myProject, summary = FALSE)
  expect_equal(names(allScns), c("4", "5", "6"))

  expect_equal(is.element("ScenarioID", names(datasheet(myLib, c("RunControl", "OutputOptions"), scenario = as.numeric(names(allScns)))[[1]])), TRUE) # returns a list - each sheet contains scenario info if appropriate

  expect_equal(length(datasheet(allScns, c("RunControl", "OutputOptions"))), 2) # returns a list - each sheet contains scenario info if appropriate

  expect_warning(datasheet(myScn, "RunControl", scenario = 1), "scenario argument is ignored when ssimObject is a Scenario or list of these.", fixed = TRUE) # Warn of conflict between ssimObject and scenario arguments.
  expect_warning(datasheet(myProject, "corestime_Charts", project = 1), "project argument is ignored when ssimObject is a Project/Scenario or list of these.", fixed = TRUE) # Warn of conflict between ssimObject and project arguments.
  expect_warning(datasheet(allScns, "RunControl", scenario = 1), "scenario argument is ignored when ssimObject is a list.", fixed = TRUE) # Warn that project/scenario arguments are ignored when ssimObject is a list of Project/Scenario objects.
  # Fails in test env only
  expect_equal(runLog(myScn), "The scenario is not a result scenario: 6") # Returns message if the scenario is not a result scenario.

  # get/set properties
  name(myProject) <- "New project name"
  expect_equal(name(myProject), "New project name")

  name(myScn) <- "New scn name"
  expect_equal(name(myScn), "New scn name")

  description(myProject) <- "A new description.\nTry a linebreak." # NOTE: \n adds a linebreak to the description
  expect_equal(description(myProject)[1], "A new description.")
  description(myScn) <- "Hi"
  expect_equal(grepl("Hi", description(myScn)[1]), TRUE)

  owner(myProject) <- "Fred"
  expect_equal(owner(myProject), "Fred")
  owner(myScn) <- "Alice"
  expect_equal(owner(myScn), "Alice")

  readOnly(myProject) <- TRUE
  expect_equal(readOnly(myProject), "Yes")
  readOnly(myProject) <- FALSE
  expect_equal(readOnly(myProject), "No")
  readOnly(myScn) <- TRUE
  expect_equal(readOnly(myScn), "Yes")

  expect_equal(grepl("at", dateModified(myProject)), TRUE)
  expect_equal(grepl("at", dateModified(myScn)), TRUE)
  expect_equal(parentId(myScn), NA) # NA for scenarios that aren't results.

  expect_is(ssimLibrary(myProject), "SsimLibrary") # get parent library
  expect_is(session(myProject), "Session") # get parent session
  expect_is(ssimLibrary(myScn), "SsimLibrary") # get parent library
  expect_is(session(myScn), "Session") # get parent session
  expect_equal(projectId(project(myScn)), projectId(myScn)) # get parent project

  expect_equal(file.exists(filepath(myScn)), TRUE)
  expect_equal(filepath(myScn), filepath(myProject))
  expect_false(ssimUpdate(myScn))
  expect_false(ssimUpdate(myProject))

  # test dependency, precedence setting
  ret <- scenario(myProject, scenario = "another scn")
  targetScn <- scenario(myProject, scenario = "second scen")
  ret <- dependency(targetScn, dependency = c("other", "New scn name", "another scn")) # elements of the dependency argument are ordered from lowest to highest precedence
  expect_equal(dependency(targetScn)$name, c("another scn", "New scn name", "other"))
  ret <- dependency(targetScn, dependency = c("another scn", "New scn name")) # change the precedence of dependencies by adding them again.
  expect_equal(dependency(targetScn)$name, c("New scn name", "another scn", "other")) # now "New scn name" has highest precedence.

  # test delete - vectors of project/scenario/datasheet
  # Fails in test env
  retList <- delete(myLib, project = project(myLib)$projectId[1:2], 
                    datasheet = c("corestime_Charts", "corestime_DistributionType"), force = TRUE)
  expect_is(retList, "list")
  expect_true(retList[[1]], retList[[2]])

  ret <- delete(myLib, scenario = c(6, 7), force = TRUE)
  expect_equal(intersect(c(6, 7), scenario(myLib)$scenarioId), integer(0))
  ret <- delete(myLib, scenario = c("one", "two"), force = TRUE)
  expect_equal(intersect(c("one", "two"), scenario(myLib)$name), character(0))

  ret <- delete(myLib, project = c(1, 11), force = TRUE)
  expect_equal(intersect(c(1, 11), project(myLib)$projectId), integer(0))
  ret <- delete(myLib, project = c("copy", "copy2"), force = TRUE)
  expect_equal(intersect(c("copy", "copy2"), project(myLib)$name), logical(0))

  # Test deletions
  ret <- delete(myLib, force = TRUE)
  ret <- delete(paste0(getwd(), "/temp27.ssim"), force = TRUE) # delete a library specified by a path
  expect_error({delete(paste0(getwd(), "/temp26.ssim"), force = TRUE)}, 
               "Library not found") 
  
})

test_that("Tests of datasheet - assumes SyncroSim is installed", {
  skip_on_cran()
  myLibrary <- ssimLibrary(name = "NewLibrary.ssim", session = mySsim)
  myProject <- project(myLibrary, project = "proj")
  myScenario <- scenario(myProject, scenario = "one")
  myLibraryDataframes <- datasheet(myLibrary, summary = FALSE) # A named list of all the library datasheets for project id 2.
  expect_is(myLibraryDataframes, "list")
  expect_is(myLibraryDataframes[["corestime_Options"]], "data.frame")

  myProjectSheetNames <- subset(datasheet(myProject), scope == "project") # A dataframe of datasheet names for project id 1.
  expect_equal(names(myProjectSheetNames), c("scope", "name", "displayName"))

  myDeterministicTransitions <- suppressWarnings(datasheet(myScenario, "DeterministicTransition"))
  expect_is(myDeterministicTransitions$StateClassIDSource, "factor")
  myDeterministicTransitions <- suppressWarnings(datasheet(myScenario, "DeterministicTransition", lookupsAsFactors = FALSE)) # This option returns characters instead of factors.
  expect_is(myDeterministicTransitions$StateClassIDSource, "NULL")

  sheetName <- "stsim_StateLabelX"
  emptyTab <- datasheet(myProject, name = sheetName, empty = FALSE)
  expect_equal(nrow(emptyTab), 0)
  stateClassDefinition <- addRow(emptyTab, data.frame(Name = c("Coniferous", "Deciduous", "Mixed")))
  expect_equal(stateClassDefinition$Name, c("Coniferous", "Deciduous", "Mixed"))
  ret <- saveDatasheet(myProject, stateClassDefinition, name = sheetName) # append project scope datasheet by default
  ret <- saveDatasheet(myProject, stateClassDefinition, name = sheetName, append = FALSE, force = TRUE) # remove without prompting

  stateClassDefinition <- addRow(emptyTab, data.frame(Name = c("Grass")))
  ret <- saveDatasheet(myProject, stateClassDefinition, name = sheetName) # append project scope datasheet by default
  expect_equal(datasheet(myProject, name = sheetName)$Name, c("Coniferous", "Deciduous", "Grass", "Mixed"))

  ret <- delete(myProject, datasheet = sheetName, force = TRUE)
  expect_equal(nrow(datasheet(myProject, name = sheetName)), 0)

  ret <- delete(myLibrary, force = TRUE)
})

setwd(old_dir)
unlink(temp_dir, recursive = TRUE)
