;Noel Kennebeck

(defun begin()
  (princ "Welcome to Mastermind! Press enter to continue:")
  (read-line)
  (setq play t)
  (loop while play do
    (princ "How many colors and pegs should I use? ")
    (setq colors (read))
    (setq pegs (read))
    (printcolors)
    (setq solList (generateSolution pegs nil))
    (print-finish (+ (play solList) 1))
    (setq play (play-again))
  )
)

(defun printcolors()
  (if (eq colors 6)
    (format t "Your colors are (R)ed, (B)lue, (G)reen, (Y)ellow, (O)range, and (P)urple")
    (if (eq colors 5)
      (format t "Your colors are (R)ed, (B)lue, (G)reen, (Y)ellow, and (O)range")
      (if (eq colors 4)
        (format t "Your colors are (R)ed, (B)lue, (G)reen, and (Y)ellow")
        (if (eq colors 3)
          (format t "Your colors are (R)ed, (B)lue, and (G)reen")
          (if (eq colors 2)
            (format t "Your colors are (R)ed and (B)lue")
            (if (eq colors 1)
              (format t "Your color is (R)ed")
              (promptagain)))))))
)

(defun promptagain()
  (princ "Please enter a number between 1 and 6: ")
  (setq colors (read))
  (printcolors)
)

(defun generateSolution(pegs list)
  (setf *random-state* (make-random-state t))
  (setq rand (random colors))
  (setq retList list)
  (if (eq pegs 0)
    retList
    (if (eq rand 0)
      (generateSolution (- pegs 1) (cons "R" retList))
      (if (eq rand 1)
        (generateSolution (- pegs 1) (cons "B" retList))
        (if (eq rand 2)
          (generateSolution (- pegs  1) (cons "G" retList))
          (if (eq rand 3)
            (generateSolution (- pegs 1) (cons "Y" retList))
            (if (eq rand 4)
              (generateSolution (- pegs 1) (cons "O" retList))
              (generateSolution (- pegs 1) (cons "P" retList))))))))
)

;returns the number of guesses it took for the user to solve
(defun play(sol)
  (setq guesses 0)
  (setq unsolved t)
  (loop while unsolved do
    (format t "~%Enter your guess: ")
    (setq guess (get-line))
    (setq copysol (copy-list sol))
    (if(report guess copysol)
      (setq unsolved nil)
      (setq guesses (+ guesses 1))
    )
  )
  guesses
)

;takes in user's guess and compares to solution
(defun report(guess sol)
  (setq res (rcomp guess sol))
  (setq sol-mod (first res))
  (setq guess-mod (second res))
  (setq rplace (- pegs (list-length sol-mod)))
  (setq wplace (wcomp guess-mod sol-mod))
  (if (eq pegs rplace) t (preport rplace wplace))
)

(defun preport (rplace wplace)
  (if (eq rplace 1)
    (format t "Your guess contains 1 piece in the right place and ")
    (format t "Your guess contains ~D pieces in the right place and " rplace)
  )
  (if (eq wplace 1)
    (format t "1 piece of the correct color in the wrong place.~%")
    (format t "~D pieces of the correct color in the wrong place.~%" wplace)
  )
  nil
)

;calculates how many pegs are right color right spot
(defun rcomp(guess sol)
  (loop for i from 1 to pegs do
    (setq x (pop sol))
    (setq y (pop guess))
    (if (eq 1 (comp x y)) nil (nconc sol (list x)))
    (if (eq 1 (comp x y)) nil (nconc guess (list y)))
  )
  (list sol guess)
)

;calculates how many pegs are right color wrong spot
(defun wcomp(list sol)
  (setq wplace 0)
  (loop for y in sol do
    (setq str (pop sol))
    (loop for x in list do
      (if (eq 1 (comp str x)) (setq wplace (+ wplace 1)))
    )
  )
  wplace
)

;reports how many tries it took the user to guess the solution
(defun print-finish(x)
  (if(eq x 1)
    (format t "You guessed it in 1 go.~%")
    (format t "You guessed it in ~D goes.~%" x)))

;prompts user to play again
(defun play-again()
  (setq invalid-ans t)
  (setq pa t)
  (princ "Play again? ")
  (loop while invalid-ans do
    (setq choice (read))
    (cond ((eq 'y choice) (setq invalid-ans nil) (setq pa t))
          ((eq 'yes choice) (setq invalid-ans nil) (setq pa t))
          ((eq 'yep choice) (setq invalid-ans nil) (setq pa t))
          ((eq 'ok choice) (setq invalid-ans nil) (setq pa t))
          ((eq 'sure choice) (setq invalid-ans nil) (setq pa t))
          ((eq 'nope choice) (setq invalid-ans nil) (setq pa nil))
          ((eq 'nah choice) (setq invalid-ans nil) (setq pa nil))
          ((eq 'no choice) (setq invalid-ans nil) (setq pa nil))
          ((eq 'n choice) (setq invalid-ans nil) (setq pa nil))
          (t (princ "Invalid response, enter yes or no: ") (setq invalid-ans t))))
  pa
)

(defun comp (str1 str2) (if (equal str1 str2) 1 0))

(defun get-line () (setf text (mapcar 'string (coerce (string (read)) 'list))))

(begin)
