(require 'request)
(require 'cl)

(setq phorge-api-token "")

(setq phorge-url "")

(defun org-phorge--group-tasks-by-milestone (tasks)
  (seq-group-by (lambda (task)
                  (alist-get 'milestone-phid task))
                tasks))

(defun org-phorge--process-get-tasks (data)
  (org-phorge--group-tasks-by-milestone
   (mapcar (lambda (task)
             (let* ((fields (alist-get 'fields task))
                    (attachments (alist-get 'attachments task))
                    (milestone (--> attachments
                                    (alist-get 'projects it)
                                    (alist-get 'projectPHIDs it)
                                    (aref it 0)))
                    (column (->> attachments
                                 (alist-get 'columns)
                                 (alist-get 'boards))))
               (when milestone
                 `((id . ,(alist-get 'id task))
                   (name . ,(alist-get 'name fields))
                   (type . ,(alist-get 'custom\.rams:type fields))
                   (dev-points ,(alist-get 'custom\.rams:devpoints fields))
                   (qa-points ,(alist-get 'custom\.rams:qapoints fields))
                   (total-points ,(alist-get 'points fields))
                   (priority ,(alist-get 'name (alist-get 'priority fields)))
                   (milestone-phid ,milestone)
                   (column ,(--> (intern milestone)
                                 (alist-get it column)
                                 (--> (alist-get 'columns it)
                                      (aref it 0)
                                      (alist-get 'name it))))))))
           data)))

(defun org-phorge--process-get-milestones (data)
  (mapcar (lambda (milestone)
            (let* ((fields (alist-get 'fields milestone)))
              `((id . ,(alist-get 'id milestone))
                (phid . ,(alist-get 'phid milestone))
                (name . ,(alist-get 'name fields))
                (milestone-number ,(alist-get 'milestone fields))
                (depth . ,(alist-get 'depth fields))
                (parent-name . ,(alist-get 'name (alist-get 'parent fields)))
                (parent-id . ,(alist-get 'id (alist-get 'parent fields)))
                (parent-phid . ,(alist-get 'phid (alist-get 'parent fields))))))
          data))

(defun org-phorge--get-tasks ()
  (let* ((after "0")
         (result [])
         (props `(("api.token" . ,(symbol-value 'phorge-api-token))
                  ("queryKey" . "assigned")
                  ("attachments[columns]" . true)
                  ("attachments[projects]" . true)))
         (request-fn (lambda (after)
                       (request-response-data
                        (request (concat phorge-url "/api/maniphest.search")
                          :type "POST"
                          :sync t
                          :parser 'json-read
                          :data (if (and after (not (= (string-to-number after) 0)))
                                    (push `("after" . ,after) props)
                                  props))))))
    (while after
      (let ((data (funcall request-fn after)))
        (setq after (->> data
                         (alist-get 'result)
                         (alist-get 'cursor)
                         (alist-get 'after)))
        (setq result (concatenate 'list result (->> data
                                                    (alist-get 'result)
                                                    (alist-get 'data))))))
    (org-phorge--process-get-tasks result)))

(defun org-phorge--get-milestones ()
  (let* ((after "0")
         (result [])
         (props `(("api.token" . ,(symbol-value 'phorge-api-token))
                  ("queryKey" . "joined")
                  ("constraints[isMilestone]" . true)
                  ("attachments[ancestors]" . true)))
         (request-fn (lambda (after)
                       (request-response-data
                        (request (concat phorge-url "/api/project.search")
                          :type "POST"
                          :sync t
                          :parser 'json-read
                          :data (if (and after (not (= (string-to-number after) 0)))
                                    (push `("after" . ,after) props)
                                  props))))))
    (while after
      (let ((data (funcall request-fn after)))
        (setq after (->> data
                         (alist-get 'result)
                         (alist-get 'cursor)
                         (alist-get 'after)))
        (setq result (concatenate 'list result (->> data
                                                    (alist-get 'result)
                                                    (alist-get 'data))))
        (org-phorge--process-get-milestones result)))))

(defun org-phorge--get-my-tasks ()
  (with-current-buffer (get-buffer-create "*Phorge*")
    (erase-buffer)
    (insert (mapconcat (lambda (task)
                         (concat "** TODO T" (number-to-string (alist-get 'id task)) " " (alist-get 'name task)))
                       (org-phorge--get-tasks)
                       "\n"))))
