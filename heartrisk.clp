;;; Sample Run 
;;;
;;;CLIPS> (load cholesterol.clp)
;;;CLIPS> (reset)
;;;CLIPS> (run)
;;;What is your name? Bhakti
;;;Bhakti, what is your gender? female
;;;Bhakti, what is your age? 20
;;;Bhakti, do you have obesity issue? no
;;;Bhakti, are you diabetic? no
;;;Bhakti, do you have high/low BP? no
;;;Bhakti, what is your total cholesterol? 113
;;;Bhakti, what is your LDL? 70
;;;Bhakti, what is your HDL? 47
;;;Bhakti, you have a low risk of heart disease.
;;; CLIPS> 

(deftemplate attribute
   (slot name)
   (slot value))

(defrule get-name
   =>
   (printout t "What is your name? ")
   (assert (attribute (name name) (value (read)))))
   
(defrule get-gender
   (attribute (name name) (value ?name))
   (not (attribute (name gender)))
   =>
   (printout t ?name ", what is your gender? ")
   (assert (attribute (name gender) (value (read)))))

(defrule get-age
   (attribute (name name) (value ?name))
   (not (attribute (name age)))
   =>
   (printout t ?name ", what is your age? ")
   (assert (attribute (name age) (value (read)))))

(defrule get-obesity
   (attribute (name name) (value ?name))
   (not (attribute (name obesity)))
   =>
   (printout t ?name ", do you have obesity issue? ")
   (assert (attribute (name obesity) (value (read)))))

(defrule get-diabetes
   (attribute (name name) (value ?name))
   (not (attribute (name diabetes)))
   =>
   (printout t ?name ", are you diabetic? ")
   (assert (attribute (name diabetic) (value (read)))))

(defrule get-BP
   (attribute (name name) (value ?name))
   (not (attribute (name BP)))
   =>
   (printout t ?name ", do you have high/low BP? ")
   (assert (attribute (name BP) (value (read)))))

(defrule get-total-cholestorol
   (attribute (name name) (value ?name))
   (not (attribute (name total-cholesterol)))
   =>
   (printout t ?name ", what is your total cholesterol? ")
   (assert (attribute (name total-cholesterol) (value (read)))))

(defrule get-LDL
   (attribute (name name) (value ?name))
   (not (attribute (name LDL)))
   =>
   (printout t ?name ", what is your LDL? ")
   (assert (attribute (name LDL) (value (read)))))
   
(defrule get-HDL
   (attribute (name name) (value ?name))
   (not (attribute (name HDL)))
   =>
   (printout t ?name ", what is your HDL? ")
   (assert (attribute (name HDL) (value (read)))))
      
(defrule bad-answer
   (declare (salience 10))
   (or ?a <- (attribute (name gender) (value ~male&~female))
       ?a <- (attribute (name total-cholesterol | HDL)
                        (value ?value&:(or (not (numberp ?value))
                                           (<= ?value 0)))))
   =>
   (retract ?a))

(defrule compute-ratio
   (attribute (name total-cholesterol) (value ?total))
   (attribute (name HDL) (value ?hdl))
   =>
   (assert (attribute (name ratio) (value (/ ?total ?hdl)))))
   
(defrule low-ratio
   (attribute (name name) (value ?name))
   (or (and (attribute (name gender) (value male))
            (attribute (name ratio) (value ?ratio&:(< ?ratio 3.5))))
       (and (attribute (name gender) (value female))
            (attribute (name ratio) (value ?ratio&:(< ?ratio 3.0)))))
   =>
   (printout t ?name ", you have a low risk of heart disease." crlf))

(defrule moderate-ratio
   (attribute (name name) (value ?name))
   (or (and (attribute (name gender) (value male))
            (attribute (name ratio) (value ?ratio&:(>= ?ratio 3.5)&:(<= ?ratio 5.0))))
       (and (attribute (name gender) (value female))
            (attribute (name ratio) (value ?ratio&:(>= ?ratio 3.0)&:(<= ?ratio 4.4)))))
   =>
   (printout t ?name ", you have a moderate risk of heart disease." crlf))
   
(defrule high-ratio
   (attribute (name name) (value ?name))
   (or (and (attribute (name gender) (value male))
            (attribute (name ratio) (value ?ratio&:(> ?ratio 5.0))))
       (and (attribute (name gender) (value female))
            (attribute (name ratio) (value ?ratio&:(> ?ratio 4.4))))
       (and (attribute (name obesity) (value yes))
            (attribute (name BP) (value yes))
            (attribute (name diabetes) (value yes))))
   =>
   (printout t ?name ", you have a high risk of heart disease." crlf))