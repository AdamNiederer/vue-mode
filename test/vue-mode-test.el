(load-file "./vue-mode.el")
(require 'vue-mode)
(require 's)
(require 'cl-lib)

(defconst vue--lang-start-tags
  '(;; Just a "lang" attribute
    "<template lang=\"html\">\n"
    "<template lang=\"jade\">\n"
    "<template lang=\"pug\">\n"
    "<template lang=\"slim\">\n"
    "<template lang=\"slm\">\n"
    "<script lang=\"js\">\n"
    "<script lang=\"es6\">\n"
    "<script lang=\"babel\">\n"
    "<script lang=\"coffee\">\n"
    "<script lang=\"typescript\">\n"
    "<script lang=\"ts\">\n"
    "<style lang=\"css\">\n"
    "<style lang=\"stylus\">\n"
    "<style lang=\"sass\">\n"
    "<style lang=\"scss\">\n"
    "<style lang=\"less\">\n"
    ;; Arbitrary k/v before
    "<template kek=\"bur/bar.baz\" lang=\"html\">\n"
    "<template kek=\"bur/bar.baz\" lang=\"jade\">\n"
    "<template kek=\"bur/bar.baz\" lang=\"pug\">\n"
    "<template kek=\"bur/bar.baz\" lang=\"slim\">\n"
    "<template kek=\"bur/bar.baz\" lang=\"slm\">\n"
    "<script kek=\"bur/bar.baz\" lang=\"js\">\n"
    "<script kek=\"bur/bar.baz\" lang=\"es6\">\n"
    "<script kek=\"bur/bar.baz\" lang=\"babel\">\n"
    "<script kek=\"bur/bar.baz\" lang=\"coffee\">\n"
    "<script kek=\"bur/bar.baz\" lang=\"typescript\">\n"
    "<script kek=\"bur/bar.baz\" lang=\"ts\">\n"
    "<style kek=\"bur/bar.baz\" lang=\"css\">\n"
    "<style kek=\"bur/bar.baz\" lang=\"stylus\">\n"
    "<style kek=\"bur/bar.baz\" lang=\"sass\">\n"
    "<style kek=\"bur/bar.baz\" lang=\"scss\">\n"
    "<style kek=\"bur/bar.baz\" lang=\"less\">\n"
    ;; Arbitrary k/v before & after
    "<template kek=\"bur/bar.baz\" lang=\"html\" bur=\"foo.bar & kek &= bur ? baz : !!quux + 2\">\n"
    "<template kek=\"bur/bar.baz\" lang=\"jade\" bur=\"foo.bar & kek &= bur ? baz : !!quux + 2\">\n"
    "<template kek=\"bur/bar.baz\" lang=\"pug\" bur=\"foo.bar & kek &= bur ? baz : !!quux + 2\">\n"
    "<template kek=\"bur/bar.baz\" lang=\"slim\" bur=\"foo.bar & kek &= bur ? baz : !!quux + 2\">\n"
    "<template kek=\"bur/bar.baz\" lang=\"slm\" bur=\"foo.bar & kek &= bur ? baz : !!quux + 2\">\n"
    "<script kek=\"bur/bar.baz\" lang=\"js\" bur=\"foo.bar & kek &= bur ? baz : !!quux + 2\">\n"
    "<script kek=\"bur/bar.baz\" lang=\"es6\" bur=\"foo.bar & kek &= bur ? baz : !!quux + 2\">\n"
    "<script kek=\"bur/bar.baz\" lang=\"babel\" bur=\"foo.bar & kek &= bur ? baz : !!quux + 2\">\n"
    "<script kek=\"bur/bar.baz\" lang=\"coffee\" bur=\"foo.bar & kek &= bur ? baz : !!quux + 2\">\n"
    "<script kek=\"bur/bar.baz\" lang=\"typescript\" bur=\"foo.bar & kek &= bur ? baz : !!quux + 2\">\n"
    "<script kek=\"bur/bar.baz\" lang=\"ts\" bur=\"foo.bar & kek &= bur ? baz : !!quux + 2\">\n"
    "<style kek=\"bur/bar.baz\" lang=\"css\" bur=\"foo.bar & kek &= bur ? baz : !!quux + 2\">\n"
    "<style kek=\"bur/bar.baz\" lang=\"stylus\" bur=\"foo.bar & kek &= bur ? baz : !!quux + 2\">\n"
    "<style kek=\"bur/bar.baz\" lang=\"sass\" bur=\"foo.bar & kek &= bur ? baz : !!quux + 2\">\n"
    "<style kek=\"bur/bar.baz\" lang=\"scss\" bur=\"foo.bar & kek &= bur ? baz : !!quux + 2\">\n"
    "<style kek=\"bur/bar.baz\" lang=\"less\" bur=\"foo.bar & kek &= bur ? baz : !!quux + 2\">\n"
    ;; Scoped
    "<style kek=\"bur/bar.baz\" lang=\"css\" bur=\"foo.bar & kek &= bur ? baz : !!quux + 2\">\n"
    "<style kek=\"bur/bar.baz\" lang=\"stylus\" bur=\"foo.bar & kek &= bur ? baz : !!quux + 2\">\n"
    "<style kek=\"bur/bar.baz\" lang=\"sass\" bur=\"foo.bar & kek &= bur ? baz : !!quux + 2\">\n"
    "<style kek=\"bur/bar.baz\" lang=\"scss\" bur=\"foo.bar & kek &= bur ? baz : !!quux + 2\">\n"
    "<style kek=\"bur/bar.baz\" lang=\"less\" bur=\"foo.bar & kek &= bur ? baz : !!quux + 2\">\n"))

(defconst vue--start-tags
  '(;; Bare tags
    "<style>\n"
    "<script>\n"
    "<template>\n"
    "<style scoped>\n"
    ;; Arbitrary k/v
    "<template kek=\"bur/bar.baz\">\n"
    "<template kek=\"bur/bar.baz\">\n"
    "<template kek=\"bur/bar.baz\">\n"
    "<template kek=\"bur/bar.baz\">\n"
    "<template kek=\"bur/bar.baz\">\n"
    "<script kek=\"bur/bar.baz\">\n"
    "<script kek=\"bur/bar.baz\">\n"
    "<script kek=\"bur/bar.baz\">\n"
    "<script kek=\"bur/bar.baz\">\n"
    "<script kek=\"bur/bar.baz\">\n"
    "<script kek=\"bur/bar.baz\">\n"
    "<style kek=\"bur/bar.baz\">\n"
    "<style kek=\"bur/bar.baz\">\n"
    "<style kek=\"bur/bar.baz\">\n"
    "<style kek=\"bur/bar.baz\">\n"
    "<style kek=\"bur/bar.baz\">\n"
    "<template kek=\"bur/bar.baz\" bur=\"foo.bar & kek &= bur ? baz : !!quux + 2\">\n"
    "<template kek=\"bur/bar.baz\" bur=\"foo.bar & kek &= bur ? baz : !!quux + 2\">\n"
    "<template kek=\"bur/bar.baz\" bur=\"foo.bar & kek &= bur ? baz : !!quux + 2\">\n"
    "<template kek=\"bur/bar.baz\" bur=\"foo.bar & kek &= bur ? baz : !!quux + 2\">\n"
    "<template kek=\"bur/bar.baz\" bur=\"foo.bar & kek &= bur ? baz : !!quux + 2\">\n"
    "<script kek=\"bur/bar.baz\" bur=\"foo.bar & kek &= bur ? baz : !!quux + 2\">\n"
    "<script kek=\"bur/bar.baz\" bur=\"foo.bar & kek &= bur ? baz : !!quux + 2\">\n"
    "<script kek=\"bur/bar.baz\" bur=\"foo.bar & kek &= bur ? baz : !!quux + 2\">\n"
    "<script kek=\"bur/bar.baz\" bur=\"foo.bar & kek &= bur ? baz : !!quux + 2\">\n"
    "<script kek=\"bur/bar.baz\" bur=\"foo.bar & kek &= bur ? baz : !!quux + 2\">\n"
    "<script kek=\"bur/bar.baz\" bur=\"foo.bar & kek &= bur ? baz : !!quux + 2\">\n"
    "<style kek=\"bur/bar.baz\" bur=\"foo.bar & kek &= bur ? baz : !!quux + 2\">\n"
    "<style kek=\"bur/bar.baz\" bur=\"foo.bar & kek &= bur ? baz : !!quux + 2\">\n"
    "<style kek=\"bur/bar.baz\" bur=\"foo.bar & kek &= bur ? baz : !!quux + 2\">\n"
    "<style kek=\"bur/bar.baz\" bur=\"foo.bar & kek &= bur ? baz : !!quux + 2\">\n"
    "<style kek=\"bur/bar.baz\" bur=\"foo.bar & kek &= bur ? baz : !!quux + 2\">\n"))

(ert-deftest vue--re-test ()
  (dolist (tag vue--start-tags)
    (should (s-matches?
             (format vue--front-tag-regex "[a-z]+")
             tag))
    (should (not (s-matches?
                  (format vue--front-tag-lang-regex "[a-z]+" "[a-z0-9]+")
                  tag))))
  (dolist (tag vue--lang-start-tags)
    (should (not (s-matches?
                  (format vue--front-tag-regex "[a-z]+")
                  tag)))
    (should (s-matches?
             (format vue--front-tag-lang-regex "[a-z]+" "[a-z0-9]+")
             tag))))

(ert-deftest vue--crash-test ()
  (with-temp-buffer
    (insert-file-contents "test/test.vue")
    (vue-mode)))
