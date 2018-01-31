;;;rt.el -- List available program build from a cmake project

;; Copyright (C) 2018 David Vanderhaeghe


;; Author:  David Vanderhaeghe <github@dlyr.fr>
;; Version: 0.1
;; Package-Requires:  ((emacs "25") (cmake-ide "0.6") (xml "?"))
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
(require 'cmake-ide)

(defun rt-cd-run (exec &optional dir)
  "Run the 'EXEC program, in 'DIR if provided, or in exec dir."
  (interactive "fExecutable to run:")
  (let ((wd (or dir (file-name-directory exec))))
    (if (file-exists-p exec)
	(if (file-executable-p exec)
	    (progn
	      (setq default-directory wd)
	      (start-process exec "*run*" exec))
	  (message "[%s] is not an executable file." exec))
      (message "Executable file not found [%s]." exec))))

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
	      (remove-duplicates (remove nil (mapcar 'rt--get-command-and-dir targets)) :test 'command-and-dir-eq))))))))

(defun rt--get-target-list (file)
  "Return the list of executable defined in a cbp FILE."
  (rt--parse-cbp (car (xml-parse-file file))))
  
(defun rt--run-target (list index)
  "Run a target from LIST whose corresponds to position INDEX."
  (let ((target (nth index list)))
    (rt-cd-run (car target) (cadr target))))

(defun rt--get-filename ()
  "Return the first cbp file found in 'cmake-ide--get-build-dir."
  (interactive)
  (let ((filenames (file-expand-wildcards  (concatenate 'string (cmake-ide--get-build-dir) "/*.cbp"))))
    (if filenames
	(progn
	  (message "found project file [%s]" (car filenames))
	  (car filenames))
      (progn
	(message "no project file found in build-dir")
	nil))))

(defun rt-exec(arg)
  "Prompt the user to give a target name ARG and run it.  Completion use the current cbp file."
  (interactive
   (list
    (completing-read "Available Targets: " (rt--get-target-list (rt--get-filename)))))
  (rt-cd-run arg))

(defun cmake-ide--run-cmake-impl (project-dir cmake-dir)
  "Run the CMake process for PROJECT-DIR in CMAKE-DIR.  Superseed version for rt package, and CodeBlocks project generation option to have a cbp file."
  (when project-dir
    (let ((default-directory cmake-dir))
      (cmake-ide--message "Running cmake for src path %s in build path %s" project-dir cmake-dir)
      (apply 'start-process (append (list "cmake" "*cmake*" cmake-ide-cmake-command)
                                    (split-string cmake-ide-cmake-opts)
                                    (list "-DCMAKE_EXPORT_COMPILE_COMMANDS=ON -G CodeBlocks\ -\ Unix\ Makefiles" project-dir))))))

(provide 'rt)
;;; rt.el ends here

