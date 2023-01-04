(require 'denote)
(require 'denote-completing-format)

;; para las pruebas
(denote-directory-files)
(setq s "/home/lggj/Pruebas/denote/articulos/20221231T104042.org")
(setq f "/home/lggj/Pruebas/denote/principal/20221226T160441--facilitación-directa-o-indirecta__ciencia_wiki.org~")
(setq g "/home/lggj/Pruebas/denote/20221226T160441--facilitación-directa-o-indirecta__ciencia_wiki.org~")
(setq h "/home/lggj/Pruebas/denote/principal/20221226T160441__ciencia_wiki.org")


(benchmark-run 1 (let*
		 ((project-find-functions #'denote-project-find)
		  (project (project-current nil (denote-directory)))
		  (dirs (list (project-root project)))
		  (all-files (project-files project dirs))	 
		  (htable (make-hash-table :test 'equal)))
	       (mapcar (lambda (file)
			 (if (and (denote-file-has-identifier-p file)
				  (not (backup-file-name-p file)))
			     (puthash
			      ;;file
			      (concat
			       (propertize
		      (truncate-string-to-width
		       (denote-completing-format-extract-subdir-from-path file)
		       denote-completing-format-subdir-width 0 ?\s "..." t)
		      'face 'denote-completing-format-faces-subdirectory)
		     (propertize
		      (truncate-string-to-width
		       (denote-extract-id-from-string file)
		       denote-completing-format-id-width 0 ?\s "..." t)
		      'face 'denote-completing-format-faces-date)
		     (propertize "--" 'face 'denote-completing-format-faces-delimiter)
		     (propertize
		      (truncate-string-to-width
		       (denote-completing-format-retrieve-filename-title file denote-completing-format-deslugiffy-title)
		       denote-completing-format-title-width 0 ?\s "..." t)
		      'face 'denote-completing-format-faces-title)
		     (propertize "__" 'face 'denote-faces-delimiter)
		     (propertize
		      (truncate-string-to-width
		       (string-join (denote-extract-keywords-from-path file) "_")
		       denote-completing-format-keywords-width 0 ?\s "..." t)
		      'face 'denote-completing-format-faces-keywords)
		     (propertize
		      (truncate-string-to-width
		       (concat (file-name-extension file t))
		       denote-completing-format-extension-width 0 ?\s "..." t)
		      'face 'denote-completing-format-faces-extension))
			      file htable)))
		       all-files)
	       ;;(completing-read "Select note: " htable)
	       htable
	       ))
(benchmark-run 1 (let*
		     ((project-find-functions #'denote-project-find)
		      (project (project-current nil (denote-directory)))
		      (dirs (list (project-root project)))
		      (all-files (project-files project dirs)))
		   (denote-completing-format-build-formated-hash-table all-files)))
(benchmark-run 1 (denote-file-prompt))




(denote-completing-format-retrieve-filename-title f )
(setq proj (project-current nil (denote-directory)))
(project-files proj)
(denote-directory-files)

(completing-read "HOLA: " (mapcar (lambda (f)
				    (expand-file-name (propertize f 'face 'denote-completing-format-faces-keywords))) (directory-files-recursively (denote-directory) directory-files-no-dot-files-regexp)))

(denote-directory)

(defvar denote-full-file-name-regexp
  (concat (denote-directory)
	  "\\(?1:.*\\)"
	  "\\(?2:[0-9]\\{8\\}\\)\\(?3:T[0-9]\\{6\\}\\)"
	  "\\(?:\\(?4:--\\)\\(?5:[[:alnum:][:nonascii:]-]*\\)\\)?"
	  "\\(?:\\(?6:__\\)\\(?7:[[:alnum:][:nonascii:]_-]*\\)\\)?"
	  "\\(?8:\\..*\\)?$"))




denote-faces--file-name-regexp

"\\(?1:[0-9]\\{8\\}\\)\\(?2:T[0-9]\\{6\\}\\)\\(?:\\(?3:--\\)\\(?4:[[:alnum:][:nonascii:]-]*\\)\\)?\\(?:\\(?5:__\\)\\(?6:[[:alnum:][:nonascii:]_-]*\\)\\)?\\(?7:\\..*\\)?$"




;; lo que falta hacer:
;; - [X] pasarle las propiedades a la tabla de un jalón de una sola búsqueda de regexp (esto
;; no se puede hacer ya que tendía que depender del orden del regexp y este no siempre es el
;; mismo, por ejemplo si se tiene archivos sin subdirectorio o sin título pero con keywords)
;; - [ ] ver cuales son las mejores prácticas para llamar a completing-read (por ejemplo,
;; como crear la tabla de completado y agregarle la clase)
;; - [ ] quitar todo el código que se repite, ver si hay forma de automatizar cosas
;; - [ ] ver como hacer para que el cálculo de las longitudes sea adaptativo
;; - [ ] template custom de toda la tabla (esto eliminaría todas las variables width y daría mucha flexibilidad, pero supongo que es dificil de imiplementar y aumenta la complejidad del mantenimiento)
;; - [X] faces custom de cada cosa
;; - [X] deslugiffy custom o no de títulos
;; - [ ] que haya o no delimitadores ("__" o "--") (esto requiere moficiar creación de tabla)

