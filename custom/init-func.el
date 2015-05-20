(defun kill-and-join-forward (&optional arg)
  "If at end of line, join with following; otherwise kill line. Deletes whitespace at join."
  (interactive "P")
  (if (and (eolp) (not (bolp)))
      (delete-indentation t)
    (kill-line arg)))


(defun back-to-indentation-or-beginning ()
  (interactive)
  (if (or (looking-back "^\s*")
          (eq last-command 'back-to-indentation-or-beginning))
      (beginning-of-line)
    (back-to-indentation)))

(defun lorem ()
  "Insert a lorem ipsum."
  (interactive)
  (insert "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do "
          "eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim"
          "ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut "
          "aliquip ex ea commodo consequat. Duis aute irure dolor in "
          "reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla "
          "pariatur. Excepteur sint occaecat cupidatat non proident, sunt in "
          "culpa qui officia deserunt mollit anim id est laborum."))


;; Duplicate start of line or region, from http://www.emacswiki.org/emacs/DuplicateStartOfLineOrRegion
(defun duplicate-start-of-line-or-region ()
  (interactive)
  (if mark-active
      (duplicate-region)
    (duplicate-start-of-line)))

(defun duplicate-start-of-line ()
  (if (bolp)
      (progn
        (end-of-line)
        (duplicate-start-of-line)
        (beginning-of-line))
    (let ((text (buffer-substring (point)
                                  (beginning-of-thing 'line))))
      (forward-line)
      (push-mark)
      (insert text)
      (open-line 1))))

(defun duplicate-region (&optional num start end)
  "Duplicates the region bounded by START and END NUM times.
If no START and END is provided, the current region-beginning and
region-end is used."
  (interactive "p")
  (save-excursion
    (let* ((start (or start (region-beginning)))
           (end (or end (region-end)))
           (region (buffer-substring start end)))
      (goto-char end)
      (dotimes (i num)
        (insert region)))))

(defun duplicate-current-line (&optional num)
  "Duplicate the current line NUM times."
  (interactive "p")
  (save-excursion
    (when (eq (point-at-eol) (point-max))
      (goto-char (point-max))
      (newline)
      (forward-char -1))
    (duplicate-region num (point-at-bol) (1+ (point-at-eol)))))


(defun duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated."
  (interactive "p")
  (if (region-active-p)
      (let ((beg (region-beginning))
            (end (region-end)))
        (duplicate-region arg beg end)
        (one-shot-keybinding "d" (λ (duplicate-region 1 beg end))))
    (duplicate-current-line arg)
    (one-shot-keybinding "d" 'duplicate-current-line)))

(defun one-shot-keybinding (key command)
  (set-temporary-overlay-map
   (let ((map (make-sparse-keymap)))
     (define-key map (kbd key) command)
     map) t))



(setq ancane-search-at-point-wrap nil)

(defun ancane-search-at-point-func (direction)
  (interactive)
  (let* ((text (car search-ring)) newpoint)
    (when ancane-search-at-point-wrap
      (goto-char (if (= direction 1) (point-min) (point-max)))
      (setq ancane-search-at-point-wrap nil))
    (setq newpoint (search-forward text nil t direction))
    (if newpoint
        (set-mark (if (= direction 1) (- newpoint (length text))
                    (+ newpoint (length text))))
      (message "No more: %s" text) (ding)
      (setq ancane-search-at-point-wrap text))))

(defun ancane-search-at-point-forward ()
  (interactive)
  (ancane-search-at-point-func 1))

(defun ancane-search-at-point-backwards ()
  (interactive)
  (ancane-search-at-point-func -1))

(defun yank-thing-into-search ()
  (interactive)
  (let ((text (if mark-active
                  (buffer-substring-no-properties (region-beginning)(region-end))
                (or (current-word) ""))))
    (when (> (length text) 0) (isearch-update-ring text) (setq ancane-search-at-point-wrap nil)
          (ancane-search-at-point-forward))))

;; New buffer
(defun new-empty-buffer ()
  (interactive)
  (switch-to-buffer (generate-new-buffer "untitled"))
  (funcall (and initial-major-mode))
  (setq buffer-offer-save t)
  )

(defun delete-current-buffer-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

(defun split-and-goto-window-below ()
  (interactive)
  (split-window-below)
  (other-window 1))

(defun split-and-goto-window-right ()
  (interactive)
  (split-window-right)
  (other-window 1))

(defun just-no-space ()
  (interactive)
  (delete-indentation)
  (delete-horizontal-space))

(defun delete-indentation-one-space ()
  (interactive)
  (just-no-space)
  (just-one-space))

(defun create-scala-tags ()
  (interactive)
  (when (projectile-project-p)
    (let ((working-dir default-directory)
          (dir-name (projectile-project-root))
          (tags-revert-without-query t))
      (setq default-directory dir-name)
      (eshell-command
       (format
        (concat
         "find %s -not -path \""
         dir-name
         ".ensime_cache/*\""
         " -not -path \""
         dir-name
         "./target/*\""
         " -not -path \""
         dir-name
         "./.git/*\""
         " -type f -iname \"*.scala\" | etags --regex=%s -"
         )
        dir-name
        (concat "@" (expand-file-name "~") "/etags.scala")))
      (message dir-name)
      (visit-tags-table default-directory nil)
      (setq default-directory working-dir))
    ))

(defun what-position-percentage ()
  (interactive)
  (message
   (number-to-string
    (truncate
     (* 100
        (/
         (float (line-number-at-pos))
         (count-lines (point-min) (point-max))))))))

(defun describe-foo-at-point ()
  ;;; http://www.emacswiki.org/emacs/DescribeThingAtPoint
  "Show the documentation of the Elisp function and variable near point.
        This checks in turn:
        -- for a function name where point is
        -- for a variable name where point is
        -- for a surrounding function call
        "
  (interactive)
  (let (sym)
    (cond ((setq sym (ignore-errors
                       (with-syntax-table emacs-lisp-mode-syntax-table
                         (save-excursion
                           (or (not (zerop (skip-syntax-backward "_w")))
                               (eq (char-syntax (char-after (point))) ?w)
                               (eq (char-syntax (char-after (point))) ?_)
                               (forward-sexp -1))
                           (skip-chars-forward "`'")
                           (let ((obj (read (current-buffer))))
                             (and (symbolp obj) (fboundp obj) obj))))))
           (describe-function sym))
          ((setq sym (variable-at-point)) (describe-variable sym))
          ((setq sym (function-at-point)) (describe-function sym)))))


(require 'popup-switcher)

(defun popup-test ()
  (interactive)
  (let* ((popup-items '(
                        ".gitignore"
                        ".travis.yml"
                        "CONTRIBUTING.md"
                        "README.md"
                        "TODO.txt"
                        "concurrent/src/main/scala/scalaz/concurrent/Actor.scala"
                        "concurrent/src/main/scala/scalaz/concurrent/Atomic.scala"
                        "concurrent/src/main/scala/scalaz/concurrent/BooleanLatch.scala"
                        "concurrent/src/main/scala/scalaz/concurrent/Chan.scala"
                        "concurrent/src/main/scala/scalaz/concurrent/Concurrent.scala"
                        "concurrent/src/main/scala/scalaz/concurrent/Future.scala"
                        "concurrent/src/main/scala/scalaz/concurrent/MVar.scala"
                        "concurrent/src/main/scala/scalaz/concurrent/PhasedLatch.scala"
                        "concurrent/src/main/scala/scalaz/concurrent/Run.scala"
                        "concurrent/src/main/scala/scalaz/concurrent/Strategy.scala"
                        "concurrent/src/main/scala/scalaz/concurrent/Task.scala"
                        "concurrent/src/main/scala/scalaz/concurrent/Timer.scala"
                        "core/src/main/scala/scalaz/Adjunction.scala"
                        "core/src/main/scala/scalaz/Align.scala"
                        "core/src/main/scala/scalaz/Alpha.scala"
                        "core/src/main/scala/scalaz/Applicative.scala"
                        "core/src/main/scala/scalaz/ApplicativePlus.scala"
                        "core/src/main/scala/scalaz/Apply.scala"
                        "core/src/main/scala/scalaz/Arrow.scala"
                        "core/src/main/scala/scalaz/Associative.scala"
                        "core/src/main/scala/scalaz/Bias.scala"
                        "core/src/main/scala/scalaz/Bifoldable.scala"
                        "core/src/main/scala/scalaz/Bifunctor.scala"
                        "core/src/main/scala/scalaz/BijectionT.scala"
                        "core/src/main/scala/scalaz/Bind.scala"
                        "core/src/main/scala/scalaz/Bitraverse.scala"
                        "core/src/main/scala/scalaz/CaseInsensitive.scala"
                        "core/src/main/scala/scalaz/Catchable.scala"
                        "core/src/main/scala/scalaz/Category.scala"
                        "core/src/main/scala/scalaz/Choice.scala"
                        "core/src/main/scala/scalaz/Cobind.scala"
                        "core/src/main/scala/scalaz/Codensity.scala"
                        "core/src/main/scala/scalaz/Cofree.scala"
                        "core/src/main/scala/scalaz/Cokleisli.scala"
                        "core/src/main/scala/scalaz/Comonad.scala"
                        "core/src/main/scala/scalaz/ComonadStore.scala"
                        "core/src/main/scala/scalaz/ComonadTrans.scala"
                        "core/src/main/scala/scalaz/Compose.scala"
                        "core/src/main/scala/scalaz/Composition.scala"
                        "core/src/main/scala/scalaz/Const.scala"
                        "core/src/main/scala/scalaz/Contravariant.scala"
                        "core/src/main/scala/scalaz/ContravariantCoyoneda.scala"
                        "core/src/main/scala/scalaz/Coproduct.scala"
                        "core/src/main/scala/scalaz/Cord.scala"
                        "core/src/main/scala/scalaz/Coyoneda.scala"
                        "core/src/main/scala/scalaz/Cozip.scala"
                        "core/src/main/scala/scalaz/DList.scala"
                        "core/src/main/scala/scalaz/Dequeue.scala"
                        "core/src/main/scala/scalaz/Diev.scala"
                        "core/src/main/scala/scalaz/Digit.scala"
                        "core/src/main/scala/scalaz/Distributive.scala"
                        "core/src/main/scala/scalaz/Dual.scala"
                        "core/src/main/scala/scalaz/Either.scala"
                        "core/src/main/scala/scalaz/Either3.scala"
                        "core/src/main/scala/scalaz/EitherT.scala"
                        "core/src/main/scala/scalaz/Endo.scala"
                        "core/src/main/scala/scalaz/Endomorphic.scala"
                        "core/src/main/scala/scalaz/Enum.scala"
                        "core/src/main/scala/scalaz/EphemeralStream.scala"
                        "core/src/main/scala/scalaz/Equal.scala"
                        "core/src/main/scala/scalaz/FingerTree.scala"
                        "core/src/main/scala/scalaz/Foldable.scala"
                        "core/src/main/scala/scalaz/Foldable1.scala"
                        "core/src/main/scala/scalaz/Forall.scala"
                        "core/src/main/scala/scalaz/Free.scala"
                        "core/src/main/scala/scalaz/FreeAp.scala"
                        "core/src/main/scala/scalaz/Functor.scala"
                        "core/src/main/scala/scalaz/Generator.scala"
                        "core/src/main/scala/scalaz/Heap.scala"
                        "core/src/main/scala/scalaz/IList.scala"
                        "core/src/main/scala/scalaz/ISet.scala"
                        "core/src/main/scala/scalaz/Id.scala"
                        "core/src/main/scala/scalaz/IdT.scala"
                        "core/src/main/scala/scalaz/ImmutableArray.scala"
                        "core/src/main/scala/scalaz/IndexedContsT.scala"
                        "core/src/main/scala/scalaz/Inject.scala"
                        "core/src/main/scala/scalaz/Injective.scala"
                        "core/src/main/scala/scalaz/InvariantFunctor.scala"
                        "core/src/main/scala/scalaz/IsEmpty.scala"
                        "core/src/main/scala/scalaz/Isomorphism.scala"
                        "core/src/main/scala/scalaz/Kan.scala"
                        "core/src/main/scala/scalaz/Kleisli.scala"
                        "core/src/main/scala/scalaz/LazyEither.scala"
                        "core/src/main/scala/scalaz/LazyEitherT.scala"
                        "core/src/main/scala/scalaz/LazyOption.scala"
                        "core/src/main/scala/scalaz/LazyOptionT.scala"
                        "core/src/main/scala/scalaz/LazyTuple.scala"
                        "core/src/main/scala/scalaz/Leibniz.scala"
                        "core/src/main/scala/scalaz/Lens.scala"
                        "core/src/main/scala/scalaz/Liskov.scala"
                        "core/src/main/scala/scalaz/ListT.scala"
                        "core/src/main/scala/scalaz/Map.scala"
                        "core/src/main/scala/scalaz/Maybe.scala"
                        "core/src/main/scala/scalaz/MaybeT.scala"
                        "core/src/main/scala/scalaz/Memo.scala"
                        "core/src/main/scala/scalaz/Monad.scala"
                        "core/src/main/scala/scalaz/MonadError.scala"
                        "core/src/main/scala/scalaz/MonadListen.scala"
                        "core/src/main/scala/scalaz/MonadPlus.scala"
                        "core/src/main/scala/scalaz/MonadReader.scala"
                        "core/src/main/scala/scalaz/MonadState.scala"
                        "core/src/main/scala/scalaz/MonadTell.scala"
                        "core/src/main/scala/scalaz/MonadTrans.scala"
                        "core/src/main/scala/scalaz/Monoid.scala"
                        "core/src/main/scala/scalaz/MonoidCoproduct.scala"
                        "core/src/main/scala/scalaz/Name.scala"
                        "core/src/main/scala/scalaz/NaturalTransformation.scala"
                        "core/src/main/scala/scalaz/NonEmptyList.scala"
                        "core/src/main/scala/scalaz/Nondeterminism.scala"
                        "core/src/main/scala/scalaz/NotNothing.scala"
                        "core/src/main/scala/scalaz/NullArgument.scala"
                        "core/src/main/scala/scalaz/NullResult.scala"
                        "core/src/main/scala/scalaz/OneAnd.scala"
                        "core/src/main/scala/scalaz/OneOr.scala"
                        "core/src/main/scala/scalaz/OptionT.scala"
                        "core/src/main/scala/scalaz/Optional.scala"
                        "core/src/main/scala/scalaz/Order.scala"
                        "core/src/main/scala/scalaz/Ordering.scala"
                        "core/src/main/scala/scalaz/PLens.scala"
                        "core/src/main/scala/scalaz/Plus.scala"
                        "core/src/main/scala/scalaz/PlusEmpty.scala"
                        "core/src/main/scala/scalaz/ProChoice.scala"
                        "core/src/main/scala/scalaz/Product.scala"
                        "core/src/main/scala/scalaz/Profunctor.scala"
                        "core/src/main/scala/scalaz/ReaderWriterStateT.scala"
                        "core/src/main/scala/scalaz/Reducer.scala"
                        "core/src/main/scala/scalaz/Representable.scala"
                        "core/src/main/scala/scalaz/Scalaz.scala"
                        "core/src/main/scala/scalaz/Semigroup.scala"
                        "core/src/main/scala/scalaz/Show.scala"
                        "core/src/main/scala/scalaz/Split.scala"
                        "core/src/main/scala/scalaz/State.scala"
                        "core/src/main/scala/scalaz/StateT.scala"
                        "core/src/main/scala/scalaz/StoreT.scala"
                        "core/src/main/scala/scalaz/StreamT.scala"
                        "core/src/main/scala/scalaz/Strong.scala"
                        "core/src/main/scala/scalaz/Tag.scala"
                        "core/src/main/scala/scalaz/Tags.scala"
                        "core/src/main/scala/scalaz/These.scala"
                        "core/src/main/scala/scalaz/Traverse.scala"
                        "core/src/main/scala/scalaz/Traverse1.scala"
                        "core/src/main/scala/scalaz/Tree.scala"
                        "core/src/main/scala/scalaz/TreeLoc.scala"
                        "core/src/main/scala/scalaz/Unapply.scala"
                        "core/src/main/scala/scalaz/UnwriterT.scala"
                        "core/src/main/scala/scalaz/Unzip.scala"
                        "core/src/main/scala/scalaz/Validation.scala"
                        "core/src/main/scala/scalaz/WriterT.scala"
                        "core/src/main/scala/scalaz/Yoneda.scala"
                        "core/src/main/scala/scalaz/Zap.scala"
                        "core/src/main/scala/scalaz/Zip.scala"
                        "core/src/main/scala/scalaz/Zipper.scala"
                        "core/src/main/scala/scalaz/package.scala"
                        "core/src/main/scala/scalaz/std/AllFunctions.scala"
                        "core/src/main/scala/scalaz/std/AllInstances.scala"
                        "core/src/main/scala/scalaz/std/AnyVal.scala"
                        "core/src/main/scala/scalaz/std/Either.scala"
                        "core/src/main/scala/scalaz/std/Function.scala"
                        "core/src/main/scala/scalaz/std/Future.scala"
                        "core/src/main/scala/scalaz/std/Iterable.scala"
                        "core/src/main/scala/scalaz/std/List.scala"
                        "core/src/main/scala/scalaz/std/Map.scala"
                        "core/src/main/scala/scalaz/std/Option.scala"
                        "core/src/main/scala/scalaz/std/PartialFunction.scala"
                        "core/src/main/scala/scalaz/std/Set.scala"
                        "core/src/main/scala/scalaz/std/SortedMap.scala"
                        "core/src/main/scala/scalaz/std/Stream.scala"
                        "core/src/main/scala/scalaz/std/String.scala"
                        "core/src/main/scala/scalaz/std/Try.scala"
                        "core/src/main/scala/scalaz/std/Tuple.scala"
                        "core/src/main/scala/scalaz/std/TypeConstraint.scala"
                        "core/src/main/scala/scalaz/std/Vector.scala"
                        "core/src/main/scala/scalaz/std/java/Enum.scala"
                        "core/src/main/scala/scalaz/std/java/Throwable.scala"
                        "core/src/main/scala/scalaz/std/java/math/BigInteger.scala"
                        "core/src/main/scala/scalaz/std/java/util/concurrent/Callable.scala"
                        "core/src/main/scala/scalaz/std/java/util/map.scala"
                        "core/src/main/scala/scalaz/std/math/BigDecimal.scala"
                        "core/src/main/scala/scalaz/std/math/BigInt.scala"
                        "core/src/main/scala/scalaz/std/math/OrderingInstances.scala"
                        "core/src/main/scala/scalaz/std/package.scala"
                        "core/src/main/scala/scalaz/syntax/AlignSyntax.scala"
                        "core/src/main/scala/scalaz/syntax/ApplicativeBuilder.scala"
                        "core/src/main/scala/scalaz/syntax/ApplicativePlusSyntax.scala"
                        "core/src/main/scala/scalaz/syntax/ApplicativeSyntax.scala"
                        "core/src/main/scala/scalaz/syntax/ApplySyntax.scala"
                        "core/src/main/scala/scalaz/syntax/ArrowSyntax.scala"
                        "core/src/main/scala/scalaz/syntax/AssociativeSyntax.scala"
                        "core/src/main/scala/scalaz/syntax/BifoldableSyntax.scala"
                        "core/src/main/scala/scalaz/syntax/BifunctorSyntax.scala"
                        "core/src/main/scala/scalaz/syntax/BindSyntax.scala"
                        "core/src/main/scala/scalaz/syntax/BitraverseSyntax.scala"
                        "core/src/main/scala/scalaz/syntax/CatchableSyntax.scala"
                        "core/src/main/scala/scalaz/syntax/CategorySyntax.scala"
                        "core/src/main/scala/scalaz/syntax/ChoiceSyntax.scala"
                        "core/src/main/scala/scalaz/syntax/CobindSyntax.scala"
                        "core/src/main/scala/scalaz/syntax/ComonadSyntax.scala"
                        "core/src/main/scala/scalaz/syntax/ComposeSyntax.scala"
                        "core/src/main/scala/scalaz/syntax/ContravariantSyntax.scala"
                        "core/src/main/scala/scalaz/syntax/CozipSyntax.scala"
                        "core/src/main/scala/scalaz/syntax/EitherOps.scala"
                        "core/src/main/scala/scalaz/syntax/EnumSyntax.scala"
                        "core/src/main/scala/scalaz/syntax/EqualSyntax.scala"
                        "core/src/main/scala/scalaz/syntax/Foldable1Syntax.scala"
                        "core/src/main/scala/scalaz/syntax/FoldableSyntax.scala"
                        "core/src/main/scala/scalaz/syntax/FunctorSyntax.scala"
                        "core/src/main/scala/scalaz/syntax/IdOps.scala"
                        "core/src/main/scala/scalaz/syntax/InvariantFunctorSyntax.scala"
                        "core/src/main/scala/scalaz/syntax/IsEmptySyntax.scala"
                        "core/src/main/scala/scalaz/syntax/KleisliOps.scala"
                        "core/src/main/scala/scalaz/syntax/MaybeOps.scala"
                        "core/src/main/scala/scalaz/syntax/MonadErrorIdOps.scala"
                        "core/src/main/scala/scalaz/syntax/MonadErrorSyntax.scala"
                        "core/src/main/scala/scalaz/syntax/MonadListenSyntax.scala"
                        "core/src/main/scala/scalaz/syntax/MonadPlusSyntax.scala"
                        "core/src/main/scala/scalaz/syntax/MonadSyntax.scala"
                        "core/src/main/scala/scalaz/syntax/MonadTellSyntax.scala"
                        "core/src/main/scala/scalaz/syntax/MonoidSyntax.scala"
                        "core/src/main/scala/scalaz/syntax/NonEmptyListOps.scala"
                        "core/src/main/scala/scalaz/syntax/NondeterminismSyntax.scala"
                        "core/src/main/scala/scalaz/syntax/Ops.scala"
                        "core/src/main/scala/scalaz/syntax/OptionalSyntax.scala"
                        "core/src/main/scala/scalaz/syntax/OrderSyntax.scala"
                        "core/src/main/scala/scalaz/syntax/PlusEmptySyntax.scala"
                        "core/src/main/scala/scalaz/syntax/PlusSyntax.scala"
                        "core/src/main/scala/scalaz/syntax/ProChoiceSyntax.scala"
                        "core/src/main/scala/scalaz/syntax/ProfunctorSyntax.scala"
                        "core/src/main/scala/scalaz/syntax/ReducerOps.scala"
                        "core/src/main/scala/scalaz/syntax/SemigroupSyntax.scala"
                        "core/src/main/scala/scalaz/syntax/ShowSyntax.scala"
                        "core/src/main/scala/scalaz/syntax/SplitSyntax.scala"
                        "core/src/main/scala/scalaz/syntax/StateOps.scala"
                        "core/src/main/scala/scalaz/syntax/StrongSyntax.scala"
                        "core/src/main/scala/scalaz/syntax/Syntax.scala"
                        "core/src/main/scala/scalaz/syntax/TagOps.scala"
                        "core/src/main/scala/scalaz/syntax/TheseOps.scala"
                        "core/src/main/scala/scalaz/syntax/Traverse1Syntax.scala"
                        "core/src/main/scala/scalaz/syntax/TraverseSyntax.scala"
                        "core/src/main/scala/scalaz/syntax/TreeOps.scala"
                        "core/src/main/scala/scalaz/syntax/UnzipSyntax.scala"
                        "core/src/main/scala/scalaz/syntax/ValidationOps.scala"
                        "core/src/main/scala/scalaz/syntax/WriterOps.scala"
                        "core/src/main/scala/scalaz/syntax/ZipSyntax.scala"
                        "core/src/main/scala/scalaz/syntax/package.scala"
                        "core/src/main/scala/scalaz/syntax/std/BooleanOps.scala"
                        "core/src/main/scala/scalaz/syntax/std/EitherOps.scala"
                        "core/src/main/scala/scalaz/syntax/std/Function1Ops.scala"
                        "core/src/main/scala/scalaz/syntax/std/Function2Ops.scala"
                        "core/src/main/scala/scalaz/syntax/std/ListOps.scala"
                        "core/src/main/scala/scalaz/syntax/std/MapOps.scala"
                        "core/src/main/scala/scalaz/syntax/std/OptionIdOps.scala"
                        "core/src/main/scala/scalaz/syntax/std/OptionOps.scala"
                        "core/src/main/scala/scalaz/syntax/std/SortedMapOps.scala"
                        "core/src/main/scala/scalaz/syntax/std/StreamOps.scala"
                        "core/src/main/scala/scalaz/syntax/std/StringOps.scala"
                        "core/src/main/scala/scalaz/syntax/std/ToAllStdOps.scala"
                        "core/src/main/scala/scalaz/syntax/std/TryOps.scala"
                        "core/src/main/scala/scalaz/syntax/std/VectorOps.scala"
                        "core/src/main/scala/scalaz/syntax/std/package.scala"
                        "effect/src/main/scala/scalaz/effect/Dup.scala"
                        "effect/src/main/scala/scalaz/effect/Effect.scala"
                        "effect/src/main/scala/scalaz/effect/FinalizerHandle.scala"
                        "effect/src/main/scala/scalaz/effect/IO.scala"
                        "effect/src/main/scala/scalaz/effect/IORef.scala"
                        "effect/src/main/scala/scalaz/effect/IoExceptionOr.scala"
                        "effect/src/main/scala/scalaz/effect/LiftControlIO.scala"
                        "effect/src/main/scala/scalaz/effect/LiftIO.scala"
                        "effect/src/main/scala/scalaz/effect/MonadCatchIO.scala"
                        "effect/src/main/scala/scalaz/effect/MonadControlIO.scala"
                        "effect/src/main/scala/scalaz/effect/MonadIO.scala"
                        "effect/src/main/scala/scalaz/effect/RefCountedFinalizer.scala"
                        "effect/src/main/scala/scalaz/effect/RegionT.scala"
                        "effect/src/main/scala/scalaz/effect/Resource.scala"
                        "effect/src/main/scala/scalaz/effect/ST.scala"
                        "effect/src/main/scala/scalaz/effect/SafeApp.scala"
                        "effect/src/main/scala/scalaz/effect/World.scala"
                        "effect/src/main/scala/scalaz/std/effect/AllEffectInstances.scala"
                        "effect/src/main/scala/scalaz/std/effect/Closeable.scala"
                        "effect/src/main/scala/scalaz/std/effect/Future.scala"
                        "effect/src/main/scala/scalaz/std/effect/sql/Connection.scala"
                        "effect/src/main/scala/scalaz/std/effect/sql/PreparedStatement.scala"
                        "effect/src/main/scala/scalaz/std/effect/sql/ResultSet.scala"
                        "effect/src/main/scala/scalaz/std/effect/sql/Statement.scala"
                        "effect/src/main/scala/scalaz/syntax/effect/EffectSyntax.scala"
                        "effect/src/main/scala/scalaz/syntax/effect/IdOps.scala"
                        "effect/src/main/scala/scalaz/syntax/effect/LiftControlIOSyntax.scala"
                        "effect/src/main/scala/scalaz/syntax/effect/LiftIOSyntax.scala"
                        "effect/src/main/scala/scalaz/syntax/effect/MonadCatchIOSyntax.scala"
                        "effect/src/main/scala/scalaz/syntax/effect/MonadControlIOSyntax.scala"
                        "effect/src/main/scala/scalaz/syntax/effect/MonadIOSyntax.scala"
                        "effect/src/main/scala/scalaz/syntax/effect/ResourceSyntax.scala"
                        "effect/src/main/scala/scalaz/syntax/effect/package.scala"
                        "etc/CONTRIBUTORS"
                        "etc/CREDITS"
                        "etc/LICENCE"
                        "example/src/main/scala/scalaz/example/AdjunctUsage.scala"
                        "example/src/main/scala/scalaz/example/ApplyUsage.scala"
                        "example/src/main/scala/scalaz/example/ArrowUsage.scala"
                        "example/src/main/scala/scalaz/example/BifunctorUsage.scala"
                        "example/src/main/scala/scalaz/example/CaseInsensitiveUsage.scala"
                        "example/src/main/scala/scalaz/example/ContravariantCoyonedaUsage.scala"
                        "example/src/main/scala/scalaz/example/DirectTypeClassUsage.scala"
                        "example/src/main/scala/scalaz/example/EndoUsage.scala"
                        "example/src/main/scala/scalaz/example/EnumUsage.scala"
                        "example/src/main/scala/scalaz/example/FingerTreeUsage.scala"
                        "example/src/main/scala/scalaz/example/Foldable1Usage.scala"
                        "example/src/main/scala/scalaz/example/FoldableUsage.scala"
                        "example/src/main/scala/scalaz/example/FreeCoyoUsage.scala"
                        "example/src/main/scala/scalaz/example/FunctorUsage.scala"
                        "example/src/main/scala/scalaz/example/IListUsage.scala"
                        "example/src/main/scala/scalaz/example/IsomorphismUsage.scala"
                        "example/src/main/scala/scalaz/example/IterateeUsage.scala"
                        "example/src/main/scala/scalaz/example/KleisliUsage.scala"
                        "example/src/main/scala/scalaz/example/MixedBag.scala"
                        "example/src/main/scala/scalaz/example/NameNeedValueUsage.scala"
                        "example/src/main/scala/scalaz/example/NewTypeUsage.scala"
                        "example/src/main/scala/scalaz/example/PartiallyApplied.scala"
                        "example/src/main/scala/scalaz/example/ReaderWriterStateTUsage.scala"
                        "example/src/main/scala/scalaz/example/STUsage.scala"
                        "example/src/main/scala/scalaz/example/StateTUsage.scala"
                        "example/src/main/scala/scalaz/example/StringUsage.scala"
                        "example/src/main/scala/scalaz/example/SyntaxUsage.scala"
                        "example/src/main/scala/scalaz/example/TagUsage.scala"
                        "example/src/main/scala/scalaz/example/TrampolineUsage.scala"
                        "example/src/main/scala/scalaz/example/TraverseUsage.scala"
                        "example/src/main/scala/scalaz/example/UnapplyInference.scala"
                        "example/src/main/scala/scalaz/example/WordCount.scala"
                        "example/src/main/scala/scalaz/example/WriterUsage.scala"
                        "example/src/main/scala/scalaz/example/concurrent/ChanUsage.scala"
                        "example/src/main/scala/scalaz/example/concurrent/MVarUsage.scala"
                        "example/src/main/scala/scalaz/example/transformers/typecheck/TypeCheckerWithExplicitTypes.scala"
                        "example/src/main/scala/scalaz/example/transformers/typecheck/TypeCheckerWithExplicitTypesAST.scala"
                        "example/src/main/scala/scalaz/example/transformers/typecheck/TypeCheckerWithExplicitTypes_MonadTransformers.scala"
                        "example/src/main/scala/scalaz/example/transformers/typecheck/TypeCheckerWithExplicitTypes_Monadic.scala"
                        "iteratee/src/main/scala/scalaz/iteratee/Enumeratee2T.scala"
                        "iteratee/src/main/scala/scalaz/iteratee/EnumerateeT.scala"
                        "iteratee/src/main/scala/scalaz/iteratee/EnumeratorP.scala"
                        "iteratee/src/main/scala/scalaz/iteratee/EnumeratorT.scala"
                        "iteratee/src/main/scala/scalaz/iteratee/Input.scala"
                        "iteratee/src/main/scala/scalaz/iteratee/Iteratee.scala"
                        "iteratee/src/main/scala/scalaz/iteratee/IterateeT.scala"
                        "iteratee/src/main/scala/scalaz/iteratee/StepT.scala"
                        "iteratee/src/main/scala/scalaz/iteratee/package.scala"
                        "project/GenTypeClass.scala"
                        "project/GenerateTupleW.scala"
                        "project/Util.scala"
                        "project/build.properties"
                        "project/build.scala"
                        "project/plugins.sbt"
                        "project/sonatype-resolver.sbt"
                        "sbt"
                        "scalacheck-binding/src/main/scala/scalaz/scalacheck/ScalaCheckBinding.scala"
                        "scalacheck-binding/src/main/scala/scalaz/scalacheck/ScalazArbitrary.scala"
                        "scalacheck-binding/src/main/scala/scalaz/scalacheck/ScalazProperties.scala"
                        "tests/src/test/scala/scalaz/AlignTest.scala"
                        "tests/src/test/scala/scalaz/ApplicativeTest.scala"
                        "tests/src/test/scala/scalaz/ApplyTest.scala"
                        "tests/src/test/scala/scalaz/BindTest.scala"
                        "tests/src/test/scala/scalaz/BitraverseTest.scala"
                        "tests/src/test/scala/scalaz/BooleanSyntaxTest.scala"
                        "tests/src/test/scala/scalaz/BooleanTest.scala"
                        "tests/src/test/scala/scalaz/CaseInsensitiveTest.scala"
                        "tests/src/test/scala/scalaz/CoKleisliTest.scala"
                        "tests/src/test/scala/scalaz/CodensityTest.scala"
                        "tests/src/test/scala/scalaz/CofreeTest.scala"
                        "tests/src/test/scala/scalaz/CompositionTest.scala"
                        "tests/src/test/scala/scalaz/ConstTest.scala"
                        "tests/src/test/scala/scalaz/ContravariantCoyonedaTest.scala"
                        "tests/src/test/scala/scalaz/CoproductTest.scala"
                        "tests/src/test/scala/scalaz/CordTest.scala"
                        "tests/src/test/scala/scalaz/CoyonedaTest.scala"
                        "tests/src/test/scala/scalaz/DListTest.scala"
                        "tests/src/test/scala/scalaz/DequeueTest.scala"
                        "tests/src/test/scala/scalaz/DievTest.scala"
                        "tests/src/test/scala/scalaz/DigitTest.scala"
                        "tests/src/test/scala/scalaz/DisjunctionTest.scala"
                        "tests/src/test/scala/scalaz/EitherTTest.scala"
                        "tests/src/test/scala/scalaz/EndoTest.scala"
                        "tests/src/test/scala/scalaz/EndomorphicTest.scala"
                        "tests/src/test/scala/scalaz/EphemeralStreamTest.scala"
                        "tests/src/test/scala/scalaz/FingerTreeTest.scala"
                        "tests/src/test/scala/scalaz/Foldable1Test.scala"
                        "tests/src/test/scala/scalaz/FoldableTest.scala"
                        "tests/src/test/scala/scalaz/FreeTest.scala"
                        "tests/src/test/scala/scalaz/FunctorTest.scala"
                        "tests/src/test/scala/scalaz/HeapTest.scala"
                        "tests/src/test/scala/scalaz/IListTest.scala"
                        "tests/src/test/scala/scalaz/ISetTest.scala"
                        "tests/src/test/scala/scalaz/IdSyntaxTest.scala"
                        "tests/src/test/scala/scalaz/IdTTest.scala"
                        "tests/src/test/scala/scalaz/IdTest.scala"
                        "tests/src/test/scala/scalaz/ImmutableArrayTest.scala"
                        "tests/src/test/scala/scalaz/InjectTest.scala"
                        "tests/src/test/scala/scalaz/InvariantFunctorTest.scala"
                        "tests/src/test/scala/scalaz/KleisliTest.scala"
                        "tests/src/test/scala/scalaz/LazyEitherTTest.scala"
                        "tests/src/test/scala/scalaz/LazyEitherTest.scala"
                        "tests/src/test/scala/scalaz/LazyOptionTTest.scala"
                        "tests/src/test/scala/scalaz/LazyOptionTest.scala"
                        "tests/src/test/scala/scalaz/LazyTupleTest.scala"
                        "tests/src/test/scala/scalaz/LeibnizTest.scala"
                        "tests/src/test/scala/scalaz/LiskovTest.scala"
                        "tests/src/test/scala/scalaz/ListTTest.scala"
                        "tests/src/test/scala/scalaz/MapTest.scala"
                        "tests/src/test/scala/scalaz/MaybeTTest.scala"
                        "tests/src/test/scala/scalaz/MaybeTests.scala"
                        "tests/src/test/scala/scalaz/MonadPlusTest.scala"
                        "tests/src/test/scala/scalaz/MonadTransTest.scala"
                        "tests/src/test/scala/scalaz/MonoidCoproductTest.scala"
                        "tests/src/test/scala/scalaz/MonoidTest.scala"
                        "tests/src/test/scala/scalaz/NeedTest.scala"
                        "tests/src/test/scala/scalaz/NonEmptyListTest.scala"
                        "tests/src/test/scala/scalaz/OneAndTest.scala"
                        "tests/src/test/scala/scalaz/OneOrTest.scala"
                        "tests/src/test/scala/scalaz/OptionTTest.scala"
                        "tests/src/test/scala/scalaz/OptionalTest.scala"
                        "tests/src/test/scala/scalaz/OrderTest.scala"
                        "tests/src/test/scala/scalaz/OrderingTest.scala"
                        "tests/src/test/scala/scalaz/PLensTest.scala"
                        "tests/src/test/scala/scalaz/ProductTest.scala"
                        "tests/src/test/scala/scalaz/ReaderWriterStateTTest.scala"
                        "tests/src/test/scala/scalaz/SemigroupTest.scala"
                        "tests/src/test/scala/scalaz/SpecLite.scala"
                        "tests/src/test/scala/scalaz/StateTTest.scala"
                        "tests/src/test/scala/scalaz/StoreTTest.scala"
                        "tests/src/test/scala/scalaz/StreamTTest.scala"
                        "tests/src/test/scala/scalaz/SyntaxTest.scala"
                        "tests/src/test/scala/scalaz/TagTest.scala"
                        "tests/src/test/scala/scalaz/TheseTest.scala"
                        "tests/src/test/scala/scalaz/TraverseTest.scala"
                        "tests/src/test/scala/scalaz/TreeLocTest.scala"
                        "tests/src/test/scala/scalaz/TreeTest.scala"
                        "tests/src/test/scala/scalaz/UnapplyTest.scala"
                        "tests/src/test/scala/scalaz/UnwriterTTest.scala"
                        "tests/src/test/scala/scalaz/ValidationTest.scala"
                        "tests/src/test/scala/scalaz/WriterTTest.scala"
                        "tests/src/test/scala/scalaz/ZipTest.scala"
                        "tests/src/test/scala/scalaz/ZipperTest.scala"
                        "tests/src/test/scala/scalaz/concurrent/ActorTest.scala"
                        "tests/src/test/scala/scalaz/concurrent/ConcurrentTaskTest.scala"
                        "tests/src/test/scala/scalaz/concurrent/ConcurrentTest.scala"
                        "tests/src/test/scala/scalaz/concurrent/FutureTest.scala"
                        "tests/src/test/scala/scalaz/concurrent/MVarTest.scala"
                        "tests/src/test/scala/scalaz/concurrent/TaskTest.scala"
                        "tests/src/test/scala/scalaz/concurrent/TimerTest.scala"
                        "tests/src/test/scala/scalaz/effect/IOTest.scala"
                        "tests/src/test/scala/scalaz/effect/MonadCatchIOTest.scala"
                        "tests/src/test/scala/scalaz/effect/ResourceTest.scala"
                        "tests/src/test/scala/scalaz/effect/STTest.scala"
                        "tests/src/test/scala/scalaz/iteratee/Enumeratee2TTest.scala"
                        "tests/src/test/scala/scalaz/iteratee/EnumeratorPTest.scala"
                        "tests/src/test/scala/scalaz/iteratee/EnumeratorTTest.scala"
                        "tests/src/test/scala/scalaz/iteratee/InputTest.scala"
                        "tests/src/test/scala/scalaz/iteratee/IterateeTTest.scala"
                        "tests/src/test/scala/scalaz/std/AnyValTest.scala"
                        "tests/src/test/scala/scalaz/std/EitherTest.scala"
                        "tests/src/test/scala/scalaz/std/FunctionTest.scala"
                        "tests/src/test/scala/scalaz/std/FutureTest.scala"
                        "tests/src/test/scala/scalaz/std/IterableTest.scala"
                        "tests/src/test/scala/scalaz/std/LensTest.scala"
                        "tests/src/test/scala/scalaz/std/ListTest.scala"
                        "tests/src/test/scala/scalaz/std/MapTest.scala"
                        "tests/src/test/scala/scalaz/std/OptionTest.scala"
                        "tests/src/test/scala/scalaz/std/PartialFunctionTest.scala"
                        "tests/src/test/scala/scalaz/std/SetTest.scala"
                        "tests/src/test/scala/scalaz/std/SortedMapTest.scala"
                        "tests/src/test/scala/scalaz/std/StreamTest.scala"
                        "tests/src/test/scala/scalaz/std/StringTest.scala"
                        "tests/src/test/scala/scalaz/std/TryTest.scala"
                        "tests/src/test/scala/scalaz/std/TupleTest.scala"
                        "tests/src/test/scala/scalaz/std/VectorTest.scala"
                        "tests/src/test/scala/scalaz/std/java/EnumTest.scala"
                        "tests/src/test/scala/scalaz/std/java/math/BigIntegerTest.scala"
                        "tests/src/test/scala/scalaz/std/java/util/concurrent/CallableTest.scala"
                        "tests/src/test/scala/scalaz/std/math/BigIntTest.scala"
                        "version.sbt"
                        )))

    (psw-switcher
     :items-list popup-items
     :item-name-getter 'identity
     :switcher 'find-file)

    ;; (popup-menu* popup-items
    ;;              :point (point)
    ;;              :height 15
    ;;              :scroll-bar t
    ;;              :margin-left 1
    ;;              :margin-right 1
    ;;              :around t
    ;;              :isearch t
    ;;              )
    ))
