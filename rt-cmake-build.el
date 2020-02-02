;;; rt-cmake-build.el -- List available program build from a cmake project

;; Copyright (C) 2018-2020 David Vanderhaeghe


;; Author:  David Vanderhaeghe <github@dlyr.fr>
;; Version: 0.3
;; Package-Requires:  ((emacs "25") (cmake-build "1.0") (xml "?"))
;; Keywords: languages
;; URL: http://github.com/dlyr/rt


;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package get the list of executable program generated from a cmake project compile
;; and allows to launch any of these program

;;; Code:

(require 'xml)
(require 'cmake-build)

(defun rt--get-node-attrib-value (node attr)
  "Find the first node of NODE list that as the ATTR attribute and return its value."
  (let ((value (xml-get-attribute-or-nil (car node) attr)))
    (if (and (not value) node)
	  (rt--get-node-attrib-value (cdr node) attr)
      value)))

(defun rt--command-and-dir-eq (a b)
  "Compare two couple A and B."
  (and (equal (car a) (car b)) (equal (cadr a) (cadr b))))

(defun rt--get-command-and-dir (node)
  "Parse NODE to get the corresponding command and working dir.  Return nil if NODE don't represent a program."
  (let ((type (rt--get-node-attrib-value (xml-get-children node 'Option) 'type)))
    (when (string-equal type "1")
      (let ((command (rt--get-node-attrib-value (xml-get-children node 'Option) 'output))
	    (working-dir (rt--get-node-attrib-value (xml-get-children node 'Option) 'working_dir)))
	(list command working-dir)))))

(defun rt--parse-cbp (node)
  "Parse NODE, corresponding to a cbp file, and return the list of (exec working_dir)."
  (when (listp node)
    (let ((project (xml-get-children node 'Project)))
      (when project
	(let ((build (xml-get-children (car project) 'Build)))
	  (when build
	    (let ((targets (xml-get-children (car build) 'Target)))
	      (remove-duplicates (remove nil (mapcar 'rt--get-command-and-dir targets)) :test 'rt--command-and-dir-eq))))))))

(defun rt--get-target-list (file)
  "Return the list of executable defined in a cbp FILE."
  (when file
    (rt--parse-cbp (car (xml-parse-file file)))))
  
(defun rt--get-filename ()
  "Return the first cbp file found in 'cmake-build--get-build-dir."
  (cmake-build--save-project-root ()
  (let ((filenames (file-expand-wildcards  (concatenate 'string (cmake-build--get-build-dir) "/*.cbp"))))
    (if filenames
	(progn
	  (message "found project file [%s]" (car filenames))
	  (car filenames))
      (progn
	(message "no project file found in build-dir")
	nil)))))
(defun rt--get-working-directory (arg)
  "Return the working directory of ARG, if any." 
  (let* ((wd (seq-filter (lambda(x) (string= arg (car x))) (rt--get-target-list (rt--get-filename))))
	 (wd (if wd (cadr (car wd)) nil)))
    wd
    ))

(defun rt-run(arg)
  "Prompt the user to give a target name ARG and run it.  Completion use the current cbp file."
  (interactive
   (list
    (completing-read "Available Targets: " (rt--get-target-list (rt--get-filename)))))
  (let* ((wd (rt--get-working-directory arg)))
    (message "RT working directory %s" wd)
    (when arg
      (cmake-build--invoke-run2 arg wd))))

(defun rt-run-debug(arg)
  "Prompt the user to give a target name ARG and run it.  Completion use the current cbp file."
  (interactive
   (list
    (completing-read "Available Targets: " (rt--get-target-list (rt--get-filename)))))
  (let* ((wd (rt--get-working-directory arg)))
    (message "RT working directory %s" wd)
    (when arg
      (cmake-build-debug2 arg wd))))
    
(provide 'rt-cmake-build)
;;; rt-cmake-build.el ends here
