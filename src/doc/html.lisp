(defpackage #:coalton/doc/html
  (:use
   #:cl
   #:coalton/doc/base
   #:coalton/doc/model
   #:spinneret)
  (:local-nicknames
   (#:source #:coalton-impl/source)
   (#:tc #:coalton-impl/typechecker)
   (#:entry #:coalton-impl/entry)))

(in-package #:coalton/doc/html)

(defparameter *doc-css*
":root{
  --gap:28px;
  --content-gap:20px;
  --main-width:1100px;
  --bg:#ffffff;
  --fg:#1f1f1f;
  --muted:#6c6c6c;
  --border:#eeeeee;
  --code-bg:#f5f5f5;
  --code-block-bg:#1c1d21;
  --radius:8px
}
*,:before,:after{box-sizing:border-box}
html{overflow-y:scroll}
body{
  margin:0;
  font-family:-apple-system,BlinkMacSystemFont,\"Segoe UI\",Roboto,Oxygen,Ubuntu,Cantarell,\"Helvetica Neue\",Arial,sans-serif;
  font-size:18px;
  line-height:1.65;
  background:var(--bg);
  color:var(--fg)
}
a{color:inherit;text-decoration:none}
a:hover{text-decoration:underline}
.main{
  position:relative;
  min-height:100vh;
  max-width:calc(var(--main-width) + var(--gap) * 2);
  margin:auto;
  padding:var(--gap)
}
.post-two-pane{width:100%}
.post-two-pane .post-content{
  display:flex;
  gap:var(--gap);
  align-items:flex-start
}
.sidebar{
  width:320px;
  position:sticky;
  top:10px;
  max-height:calc(100vh - 20px);
  overflow:auto;
  display:flex;
  flex-direction:column;
  padding-right:12px
}
.sidebar-header{
  flex-shrink:0;
  border-bottom:1px solid var(--border);
  padding:0 0 12px 0
}
.sidebar-content{padding:12px 0 0 0}
.sidebar ul{list-style:none;margin:0;padding:0}
.sidebar li{margin:0 0 6px 0}
.main-content{flex:1;min-width:0}
.main-content > h1{margin-top:0}
h1{margin:40px 0 32px;font-size:40px;line-height:1.15}
h3{margin:32px 0 16px;font-size:24px;line-height:1.2}
h4{margin:24px 0 10px;font-size:18pt;line-height:1.2}
p{margin:0 0 16px 0}
hr{margin: 30px 0;height: 2px;background: rgb(214, 214, 214);border: 0;}
code{
  font-family:ui-monospace,SFMono-Regular,Menlo,Monaco,Consolas,\"Liberation Mono\",\"Courier New\",monospace;
  margin:0 4px;
  padding:4px 6px;
  font-size:.78em;
  line-height:1.5;
  background:var(--code-bg);
  border-radius:2px
}
.package-docstring,.docstring{white-space:pre-wrap}
pre{margin:16px 0}
pre code{
  display:block;
  margin:0;
  padding:10px;
  background:var(--code-block-bg);
  color:#d5d5d6;
  border-radius:var(--radius);
  overflow:auto;
  font-size:.85em
}
details{margin:12px 0 16px;border:0;padding:0}
summary{cursor:pointer;font-weight:600;padding:6px 0}
.instances-list{margin:8px 0 12px 20px}
.instances-item{margin:4px 0}
.symbol-search{margin-top:10px;position:relative}
.symbol-search input{
  width:100%;
  padding:8px;
  border:1px solid #ddd;
  border-radius:4px;
  font-size:.9rem
}
.search-results{
  display:none;
  position:absolute;
  left:0;
  right:0;
  top:calc(100% + 6px);
  background:var(--bg);
  border:1px solid #ddd;
  border-radius:4px;
  max-height:320px;
  overflow:auto;
  z-index:1000
}
.search-results.active{display:block}
.search-result-item{padding:10px;border-bottom:1px solid var(--border);cursor:pointer;font-size:.85rem}
.search-result-item:last-child{border-bottom:0}
.search-result-item:hover,.search-result-item.active{background:#f8f9fa}
.search-result-symbol{font-weight:700}
.search-result-type{color:#666;font-size:.8rem;margin-left:8px}
.search-result-package{color:#888;font-size:.8rem;margin-top:4px}
@media (max-width:1050px){
  .sidebar{display:none}
  .post-two-pane .post-content{display:block}
  .main{max-width:calc(820px + var(--gap) * 2)}
}")

(defparameter *doc-js*
"(function(){
  function text(el){return (el&&el.textContent||\"\").trim()}
  function buildIndex(main){
    var items=[]
    var pkg=\"\"
    var walker=document.createTreeWalker(main,NodeFilter.SHOW_ELEMENT,null,false)
    var node=walker.currentNode
    while(node){
      var tag=node.tagName
      if(tag===\"H1\"){
        var code=node.querySelector(\"code\")
        pkg=text(code)||text(node)
      }else if(tag===\"H4\"){
        var a=node.querySelector('a[href^=\"#\"]')
        if(a){
          var href=a.getAttribute(\"href\")||\"\"
          var name=text(a)
          var sub=node.querySelector(\"sub\")
          var typ=\"\"
          if(sub){
            typ=text(sub).split(\"·\")[0].trim()
          }
          if(href&&name){
            items.push({name:name,href:href,typ:typ,pkg:pkg})
          }
        }
      }
      node=walker.nextNode()
    }
    return items
  }
  function el(tag,cls,txt){
    var d=document.createElement(tag)
    if(cls)d.className=cls
    if(txt!=null)d.textContent=txt
    return d
  }
  function setup(){
    var input=document.getElementById(\"symbol-search-input\")
    var results=document.getElementById(\"search-results\")
    var main=document.querySelector(\".main-content\")
    if(!input||!results||!main)return
    var items=buildIndex(main)
    var activeIndex=-1
    function clear(){
      results.innerHTML=\"\"
      results.classList.remove(\"active\")
      activeIndex=-1
    }
    function setActive(i){
      var children=results.querySelectorAll(\".search-result-item\")
      for(var k=0;k<children.length;k++)children[k].classList.remove(\"active\")
      if(i<0||i>=children.length)return
      children[i].classList.add(\"active\")
      children[i].scrollIntoView({block:\"nearest\"})
    }
    function render(q){
      q=(q||\"\").trim().toLowerCase()
      results.innerHTML=\"\"
      activeIndex=-1
      if(!q){
        results.classList.remove(\"active\")
        return
      }
      var out=[]
      for(var i=0;i<items.length;i++){
        var it=items[i]
        if(it.name.toLowerCase().indexOf(q)!==-1){
          out.push(it)
          if(out.length>=40)break
        }
      }
      for(var j=0;j<out.length;j++){
        var it2=out[j]
        var row=el(\"div\",\"search-result-item\")
        row.tabIndex=-1
        row.dataset.href=it2.href
        row.appendChild(el(\"span\",\"search-result-symbol\",it2.name))
        if(it2.typ)row.appendChild(el(\"span\",\"search-result-type\",it2.typ))
        if(it2.pkg)row.appendChild(el(\"div\",\"search-result-package\",it2.pkg))
        row.addEventListener(\"mousedown\",(function(href){
          return function(e){
            e.preventDefault()
            window.location.hash=href.slice(1)
            clear()
            input.blur()
          }
        })(it2.href))
        results.appendChild(row)
      }
      if(out.length)results.classList.add(\"active\")
      else results.classList.remove(\"active\")
    }
    input.addEventListener(\"input\",function(){render(input.value)})
    input.addEventListener(\"focus\",function(){if(input.value.trim())render(input.value)})
    input.addEventListener(\"keydown\",function(e){
      var children=results.querySelectorAll(\".search-result-item\")
      if(e.key===\"Escape\"){
        clear()
        input.blur()
        return
      }
      if(!children.length)return
      if(e.key===\"ArrowDown\"){
        e.preventDefault()
        activeIndex=Math.min(children.length-1,activeIndex+1)
        setActive(activeIndex)
        results.classList.add(\"active\")
        return
      }
      if(e.key===\"ArrowUp\"){
        e.preventDefault()
        activeIndex=Math.max(0,activeIndex-1)
        setActive(activeIndex)
        results.classList.add(\"active\")
        return
      }
      if(e.key===\"Enter\"){
        if(activeIndex>=0&&activeIndex<children.length){
          var href=children[activeIndex].dataset.href
          if(href)window.location.hash=href.slice(1)
          clear()
          input.blur()
        }
      }
    })
    document.addEventListener(\"mousedown\",function(e){
      if(e.target===input||results.contains(e.target))return
      clear()
    })
  }
  if(document.readyState===\"loading\")document.addEventListener(\"DOMContentLoaded\",setup)
  else setup()
})();")

(defclass html-backend ()
  ((stream :initarg :stream
           :reader output-stream)
   (file-line-offsets :initform (make-hash-table :test 'equal)))
  (:documentation "A documentation output backend that writes the contents of Coalton packages in HTML format."))

(register-backend :html 'html-backend)

(defun join (strs sep)
  (cond
    ((null strs) "")
    ((null (rest strs)) (first strs))
    (t
     (reduce #'(lambda (accum str)
                 (concatenate 'string accum sep str))
             strs))))

(defmacro collect-html-reprs (backend objects &optional join-element)
  "Convert all OBJECTS to html using BACKEND, and join them with JOIN-element."
  `(join
    (loop for obj in ,objects
          collect (write-object ,backend obj))
    ,(when join-element
       `(with-html-string ,join-element))
    ))

(defun extract-href-fragment (html)
  (labels ((try (target quote)
             (let ((start (search target html)))
               (when start
                 (let* ((i (+ start (length target)))
                        (end (position quote html :start i)))
                   (when end
                     (subseq html i end)))))))
    (or (try "href=\"#"
             #\")
        (try "href='#"
             #\'))))

(defun package-name-string (pkg)
  (string (object-name pkg)))

(defun package-id (pkg)
  (format nil "~a-package" (string-downcase (package-name-string pkg))))

(defun sidebar-html (packages)
  (with-html-string
    (:aside :class "sidebar"
     (:div :class "sidebar-header"
      (:h3 :id "reference" "Reference")
      (:div :class "symbol-search"
       (:input :type "text" :id "symbol-search-input" :placeholder "Search symbols." :autocomplete "off")
       (:div :class "search-results" :id "search-results")))
     (:div :class "sidebar-content"
      (:ul
       (dolist (pkg packages)
         (:li
          (:a :href (format nil "#~a" (package-id pkg))
              (:code (package-name-string pkg))))))))))

(defmethod write-packages ((backend html-backend) packages)
  (let ((*html* (output-stream backend)))
    (with-html
      (:doctype)
      (:html :lang "en"
       (:head
        (:meta :charset "UTF-8")
        (:title "API Documentation")
        (:style (:raw *doc-css*)))
       (:body
        (:main :class "main"
         (:article :class "post-two-pane"
          (:div :class "post-content"
           (:raw (sidebar-html packages))
           (:div :class "main-content"
            (:raw (collect-html-reprs backend packages))))))
        (:script (:raw *doc-js*)))))))

(defmethod write-object ((backend html-backend) (object coalton-package))
  (let ((objects (package-objects object)))
    (with-html-string
      (:h1 :id (package-id object)
           "Package "
           (:code (object-name object)))
      (when (has-docstringp object)
        (:p :class "package-docstring" (object-docstring object)))
      (:raw
       (write-section backend objects
                      :type 'coalton-type
                      :label "Types"
                      :header-html-class "types-header"
                      :content-html-class "types-content"))
      (:raw
       (write-section backend objects
                      :type 'coalton-struct
                      :label "Structs"
                      :header-html-class "structs-header"
                      :content-html-class "structs-content"))
      (:raw
       (write-section backend objects
                      :type 'coalton-class
                      :label "Classes"
                      :header-html-class "classes-header"
                      :content-html-class "classes-content"))
      (:raw
       (write-section backend objects
                      :type 'coalton-value
                      :label "Values"
                      :header-html-class "values-header"
                      :content-html-class "values-content"))
      (:raw
       (write-section backend objects
                      :type 'coalton-macro
                      :label "Macros"
                      :header-html-class "macros-header"
                      :content-html-class "macros-content"))
      )))

(defun write-section (backend objects &key type label header-html-class content-html-class)
  (let ((objects (remove-if-not (lambda (entry)
                                  (typep entry type))
                                objects)))
    (if (null objects)
        ""
        (with-html-string
          (:div
           :class content-html-class
           (:h3 :class header-html-class label)
           (:raw (collect-html-reprs backend objects)))
          (:hr)))))

(defmethod write-object ((backend html-backend) (object coalton-object))
  (let* ((link-html (tc:with-pprint-variable-context ()
                      (object-link object)))
         (anchor-id (extract-href-fragment link-html)))
    (with-html-string
      (:h4 :id anchor-id
       (:raw link-html)
       " "
       (:sup
        (:sub
         (object-type object)
         (when (source-available-p object)
           (" · <a href=\"~a\">src</a></sub></sup>" (source-location-link backend object))))))
      (:raw (write-object-body backend object)))))

(defmethod write-object-body ((backend html-backend) (object coalton-type))
  (let ((ctor-html
          (loop :for ctor :in (coalton-type-constructors object)
                :for ctor-name := (tc:constructor-entry-name ctor)
                :for ctor-source-name := (lookup-constructor-source-name ctor-name)
                :for ctor-type := (tc:lookup-value-type entry:*global-environment* ctor-name)
                :for ctor-docstring := (source:docstring ctor)
                :collect (with-html-string
                           (when (not (null ctor-docstring))
                             (:span :class "docstring" " - ~A" ctor-docstring))))))
    (with-html-string
      (when ctor-html
        (:raw (join ctor-html "")))
      (:raw (doc-html object))
      (:raw (instances-html object)))))

(defmethod write-object-body ((backend html-backend) (object coalton-struct))
  (let ((struct-html
          (tc:with-pprint-variable-context ()
            (loop :with entry := (type-entry object)
                  :with package := (symbol-package (tc:type-entry-name entry))
                  :for (name type docstring) :in (struct-fields object)
                  :for symbol := (concatenate 'string "." name)
                  :when (eq ':external (nth-value 1 (find-symbol symbol package)))
                  :collect (with-html-string
                             (when (not (null docstring))
                               (:p :class "docstring" docstring)))))))
    (with-html-string
      (when struct-html
        (:raw struct-html))
      (:raw (doc-html object))
      (:raw (instances-html object)))))

(defmethod write-object-body ((backend html-backend) (object coalton-class))
  (let* ((ctx (class-constraints object))
         (pred (class-predicate object))
         (class-html-str
           (tc:with-pprint-variable-context ()
             (format nil "~:[~{~A ~}~;~{(~A) ~}~]~:*~:[~*~;~A ~]~A"
                     (second ctx)
                     (mapcar #'coalton/doc/markdown::to-markdown ctx)
                     (html-entities:encode-entities "⇒")
                     (coalton/doc/markdown::to-markdown pred)))))
    (with-html-string
      (:code (:raw class-html-str))
      (:raw (doc-html object))
      (:raw (instances-html object)))))

(defmethod write-object-body ((backend html-backend) (object coalton-value))
  (with-html-string
    (:code (:raw (coalton/doc/markdown::to-markdown (value-type object))))
    (:raw (doc-html object))))

(defmethod write-object-body ((backend html-backend) (object coalton-macro))
  (doc-html object))

(defun doc-html (object)
  (with-html-string
    (when (has-docstringp object)
      (:p :class "docstring"
       (object-docstring object)))))

(defun instances-html (object)
  (let ((instances (object-instances object)))
    (with-html-string
      (unless (null instances)
        (:details
         (:summary "Instances")
         (:ul :class "instances-list"
          (dolist (instance instances)
            (:li :class "instances-item"
             (:code (:raw
                     (coalton/doc/markdown::to-markdown instance)))))))))))

(defun has-docstringp (object)
  "Return T if OBJECT has a non-empty docstring."
  (and (object-docstring object) (string/= "" (object-docstring object))))
