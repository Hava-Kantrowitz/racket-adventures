;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname kantrowitz-lab6) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;; Lab 6
;; Hava Kantrowitz: hskantrowitz

;;
;; Problem 1
;;

(define-struct menu-item (type calories price qty instructions))
;; a MenuItem is a (make-menu-item String Natural Number Natural String)
;; interp:  represents a menu item in a fast food restaurant where
;;     type is the kind of item
;;     calories is the number of calories for a single item
;;     price is the price for a single item
;;     qty is the number of that kind of item in the order
;;     instructions are special instructions


;; an Order is one of
;;   empty
;;   (cons MenuItem Order)

(define FRIES (make-menu-item "fries" 15 1 52 "extra salt"))
(define APPLE (make-menu-item "apple" 10 3.50 6 "cut them sideways"))
(define CANDY (make-menu-item "candy" 5 .50 12 ""))

(define ORDER1 (cons APPLE (cons FRIES empty)))
(define ORDER2 (cons (make-menu-item "lasagna" 50 1 1 "no veggies")empty))
(define ORDER3 empty)
(define ORDER4 (cons APPLE empty))
(define ORDER5 (cons FRIES empty))

;; list-drinks: Order -> Order
;; Consumes an order and produces an order composed of any coke, coffee, tea
;; or milk in the original order

;; empty order
(check-expect (list-drinks empty)empty)
;; order without any drinks
(check-expect (list-drinks ORDER1)empty)
;; order with all 4 types of drink
(check-expect (list-drinks (cons (make-menu-item "coke" 30 1.75 2 "extra fizz")
                                 (cons (make-menu-item "coffee" 20 2.65 2 "")
                                       (cons (make-menu-item "tea" 10 1.50 2 "")
                                             (cons (make-menu-item "milk" 15 .75 2 "") empty)))))
              (cons (make-menu-item "coke" 30 1.75 2 "extra fizz")
                    (cons (make-menu-item "coffee" 20 2.65 2 "")
                          (cons (make-menu-item "tea" 10 1.50 2 "")
                                (cons (make-menu-item "milk" 15 .75 2 "") empty)))))
;; order with some drinks and some not drinks
(check-expect (list-drinks (cons FRIES (cons APPLE (cons
                                                    (make-menu-item "coke" 30 1 2 "")empty
                                                    ))))
              (cons (make-menu-item "coke" 30 1 2 "")empty))

;; correct-drinks?: Menu-item -> Boolean
(define (correct-drinks? an-item)
  (if (or(string=? (menu-item-type an-item) "coke")
         (string=? (menu-item-type an-item) "coffee")
         (string=? (menu-item-type an-item) "tea")
         (string=? (menu-item-type an-item) "milk"))
      true
      false))

(define (list-drinks an-order)
  (filter correct-drinks? an-order))

;;
;; Problem 2
;;

;; special-orders: Order -> ListOfStrings
;; consumes an order and produces a list of strings in the form
;; "item-type: instructions"

;; empty order
(check-expect (special-orders empty)empty)
;; no special instructions
(check-expect (special-orders (cons CANDY empty))(cons "candy: none" empty))
;; 1 special instruction
(check-expect (special-orders (cons CANDY (cons FRIES empty)))
              (cons "candy: none" (cons "fries: extra salt" empty)))
;; 2+ special instructions
(check-expect (special-orders (cons APPLE (cons FRIES empty)))
              (cons "apple: cut them sideways" (cons "fries: extra salt" empty)))

;; grab-instructions: Menu-item -> String
;; consumes a menu-item and produces its special order in the form "item-type: instructions"

(define (grab-instructions an-item)
  (if (string=? (menu-item-instructions an-item) "")
      (string-append (menu-item-type an-item) ": none")
      (string-append (menu-item-type an-item) ": " (menu-item-instructions an-item))))

(define (special-orders an-order)
  (map grab-instructions an-order))

;;
;; Problem 3
;;

;; high-calorie-items: Order Natural -> Order
;; consumes an order and a number of calories, and produces a list
;; that contains only those menu items where the calorie count for the type
;; of item is greater than the given calorie count.

(check-expect (high-calorie-items (cons APPLE (cons FRIES (cons CANDY empty))) 10)
              (cons FRIES empty))
(check-expect (high-calorie-items empty 10) empty)


(define (high-calorie-items an-order cal)
  (local [(define (high-calorie? an-item)
            (> (menu-item-calories an-item) cal))]
          (filter high-calorie? an-order)))

;;
;; Problem 4
;;

;; count-high-calorie-items: Order Natural -> Natural
;; consumes an order and number of calories, and produces the number of items in the
;; order that have more calories than the given number of calories

;; order with 1 higher and 1 lower
(check-expect (count-high-calorie-items ORDER1 11)52)
;; order with lower value
(check-expect (count-high-calorie-items ORDER2 60)0)
;; order with higher value
(check-expect (count-high-calorie-items ORDER4 7)6)
;; empty order
(check-expect (count-high-calorie-items ORDER3 50)0)
;; order equal to given calories
(check-expect (count-high-calorie-items ORDER5 15)0) 

(define (count-high-calorie-items an-order cal)
  (local [(define (count-items an-order acc)
            (cond [(empty? an-order) acc]
                  [(cons? an-order) (count-items (rest an-order) (+ (menu-item-qty (first an-order)) acc))]))]
    (count-items (high-calorie-items an-order cal) 0)))

;;
;; Problem 5
;;

(define-struct user (username messages))
;; a User is a (make-user String ListOfString)
;; interp. as a person where
;;   username is the person's email system user name
;;   messages are the email messages sent by the person

(define-struct message (sender text unread?))
;; a Message is a (make-message User String Boolean)
;; interp. as an email message where
;;   sender is the user who sent the message
;;   text is the text of the message
;;   flag is a boolean that returns true if the message hasn't read the message

;; a MailSystem is one of
;;   empty
;;   (cons User ListOfUser)

;; Mailsys: MailSystem
;; remembers the information in a user system
(define Mailsys empty)

;;
;; Problem 6
;;

;; add-user: String -> void
;; consumes a username and produces void
;; EFFECT: add a new user with the given username to Mailsys

(define (add-user username)
  (set! Mailsys (add-user-list username Mailsys)))

(define (add-user-list username msys)
  (cons (make-user username empty) msys))

;;
;; Problem 7
;;

;; send-email: String String String -> void 
;; consumes the name of the sender of an email, the name of the recipient of
;; the email, and the text of an email message, and produces void
;; EFFECT: store a new unread message in the recipient's mailbox

(define Mailsys2 (cons (make-user "Simon" empty) (cons (make-user "Hava" empty) empty)))

(define (send-email sender-name recip-name message-text)
  (set! Mailsys2 (send-email-list sender-name recip-name message-text Mailsys2)))

(define (send-email-list sender-name recip-name message-text mail-sys)
  (begin (if (string=? (user-username (first mail-sys)) recip-name)
                              (set-user-messages! (first mail-sys) (cons (make-message (make-user sender-name empty)
                                            message-text true) empty))
                              (send-email-list sender-name recip-name message-text (rest mail-sys)))
                                 Mailsys2))


;;
;; Problem 8
;;

;; get-unread-messages-and-mark-read: String -> ListOfString
;; consumes a username and produces a list of the unread messages in the inbox of the user with
;; the given username. Assume the username exists within the system
;; EFFECT: All emails in the user's inbox are now flagged as read

(define Mailsys3 (list (make-user "Simon" empty)
                       (make-user "Hava" (list (make-message "Jake" "hi" true)
                                                     (make-message "Joe" "hola" true)
                                                     (make-message "Kyle" "bonjour" false)
                                                     (make-message "Dean" "shalom" true)))))

(define (get-unread-messages-and-mark-read username)
  (set! Mailsys3 (mark-read-list username Mailsys3)))

(define (mark-read-list name mail-sys)
  (cond [(empty? mail-sys) empty]
        [(cons? mail-sys) (if (string=? (user-username (first mail-sys)) name)
                          (begin
                            (mark-read (first mail-sys))
                            (print-list (first mail-sys))
                            mail-sys)
                          (mark-read-list name (rest mail-sys)))]))

(define (mark-read alom)
       (if (user? alom) (begin
                       (set-message-unread?! (first (user-messages alom)) false)
                       (set-message-unread?! (first (rest (user-messages alom))) false)
                       (mark-read (rest (user-messages alom))))
           empty))

(define (print-list alom)
  (if (user? alom) (cons (message-text (first (user-messages alom)))
                            (print-list (rest (user-messages alom))))
      empty))
                              

;;
;; Problem 9
;;

;; most-messages: -> User
;; produces the user in the mailsystem with the largest number of messages in his/her mailbox.
;; If there are no users in the system, the function produces an appropriate error. If two or more users have the
;; most messages, the function just needs to return one of them.

(define (most-messages)
  (set! Mailsys4 (most-message-list Mailsys4)))

(define (most-messages alou)
  (cond [(empty? alou) (error "no account exists")]
        [(user? alou) (if ())]))

(define (count-message-amount lou)
  (local [(define (count-first lou acc acc2)
            (cond [(empty? lou) acc]
                  [(user? lou) (local [(define (count-second lou acc2)
                                    (cond [(empty? (first (user-messages (first lou)))) acc2]
                                     [(cons? (first (user-messages (first lou)))) (+ (+ 1 acc2)
                                                                                     (count-second (rest (user-messages (first lou))) acc2))]))]
                                 (count-second Mailsys4 0))
                               (if (> acc2 acc)
                                   
    (count-first Mailsys4 0))]))]))

;;
;; Problem 10
;;

;; Tests for previous problems:

;; Problem 6

"show Mailsys"
Mailsys
"adds user to mailsys"
(add-user "Hava")
"show Mailsys"
Mailsys

;; Problem 7
"show Mailsys2"
Mailsys2
"stores unread message in a user's inbox"
(send-email "Joe" "Hava" "Hello!")
"show Mailsys2"
Mailsys2

;; Problem 8
"show Mailsys3"
Mailsys3
"changes from unread to unread, prints list of messages"
(get-unread-messages-and-mark-read "Hava")
"show Mailsys3"
Mailsys3

;; Problem 9
"show Mailsys4"
"produces the user with the most messages: Hava"
"show Mailsys4"

"show Mailsys5"
"produces the user with the most messages (both are same): Simon"
"show Mailsys5"

"show Mailsys6"
"produces user with the most messages (no users): error"
"show Mailsys6"



                              
                                            
                                            
