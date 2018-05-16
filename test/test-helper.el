(when (require 'undercover nil t)
  (undercover "*.el" (:report-type :codecov)))
