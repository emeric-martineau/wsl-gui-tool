# WSL GUI tool

## Run a distribution

Select a stopped distribution (with red square) and click on blue arrow on toolbar

![Quick run distribution](images/screenshot/run-a-distribution.jpg)

or you can use right click and popup menu. You can run distribution with defaut command and defaut user or with specific command and user

![Run distribution](images/screenshot/run-a-distribution-with-popup-menu.jpg)

## Stop a distribution

Select a running distribution (with blue arrow) and click on red square on toolbar

![Quick stop distribution](images/screenshot/stop-a-distribution.jpg)

![Stop distribution using popup menu](images/screenshot/stop-a-distribution-with-popup-menu.jpg)

## Export a distribution

Select a distribution (running or stopped)

![Export distribution](images/screenshot/export-distribution.jpg)

then `Save file as` dialogbox appear.

The file is a tarball.

## Import a distribution

No distribution need to be selected

![Export distribution](images/screenshot/import-distribution1.jpg)

![Export distribution window](images/screenshot/import-distribution2.jpg)

You need specify a folder where data is stored from tarball file.

## Clone a distribution

Select a distribution (running or stopped)

![Clone distribution](images/screenshot/clone-distribution1.jpg)

![Clone distribution window](images/screenshot/clone-distribution2.jpg)

When you clone, you only need enter name. The output folder is automatically generated from original.

## Delete a distribution

Select a distribution (running or stopped)

![Delete distribution](images/screenshot/delete-distribution.jpg)

## Set a distribution as default

Select a distribution (running or stopped)

![Set default distribution](images/screenshot/set-default-distribution1.jpg)

Note: Default distribution have an asterisk on icon status.

## Change properties of a distribution

Select a distribution (running or stopped)

![Edit distribution](images/screenshot/edit-distribution1.jpg)

![Edit distribution using popup menu](images/screenshot/edit-distribution2.jpg)

and change what you want

![Change distribution properties](images/screenshot/edit-distribution3.jpg)

## Check job status

Because export, import, clone distribution can be long, these jobs run in background. To know status of job, check window statusbar

Job running

![Job running](images/screenshot/status-job-distribution1.jpg)

when you double click, a popup appear to cancel job

![Cancel popup](images/screenshot/status-job-distribution6.jpg)

Job was canceled

![Job canceled](images/screenshot/status-job-distribution3.jpg)

Job finish with success

![Job success](images/screenshot/status-job-distribution2.jpg)

Job finish with error

![Job error](images/screenshot/status-job-distribution4.jpg)

when you double click, a window with run command, stdout and stderr

![Log](images/screenshot/status-job-distribution5.jpg)

## Edit wslconfig file

In tool bar, click on:
![WSLConfig toolbar button](images/screenshot/edit-wslconfig-file-toolbar.jpg)

then a new window is displayed:
![WSLConfig](images/screenshot/edit-wslconfig-file)
