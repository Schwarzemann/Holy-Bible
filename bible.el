(defun bible-reader ()
  "Interactive function for reading the Bible in Emacs."
  (interactive)
  (let* ((book (completing-read "Enter the name of the book: " '("Genesis" "Exodus" "Leviticus" "Numbers" "Deuteronomy" "Joshua" "Judges" "Ruth" "1 Samuel" "2 Samuel" "1 Kings" "2 Kings" "1 Chronicles" "2 Chronicles" "Ezra" "Nehemiah" "Esther" "Job" "Psalms" "Proverbs" "Ecclesiastes" "Song of Solomon" "Isaiah" "Jeremiah" "Lamentations" "Ezekiel" "Daniel" "Hosea" "Joel" "Amos" "Obadiah" "Jonah" "Micah" "Nahum" "Habakkuk" "Zephaniah" "Haggai" "Zechariah" "Malachi")))
         (chapter (read-number (format "Enter the chapter number for %s: " book)))
         (verse (read-number (format "Enter the verse number for %s %d: " book chapter))))
    (find-file (format "~/bible/%s/%02d/%02d.txt" book chapter verse))))
