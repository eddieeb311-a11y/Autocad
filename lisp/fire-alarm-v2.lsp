;;; ============================================================================
;;; FIRE ALARM SYSTEM PLUGIN v2.0 - ГАЛЫН ДОХИОЛЛЫН СИСТЕМ
;;; Dynamic Block Support - "Fire devices" блок ашиглана
;;; Version: 2.0
;;; Author: Claude AI
;;; Standard: MNS + EN 54
;;; ============================================================================

;;; ----------------------------------------------------------------------------
;;; GLOBAL VARIABLES - ГЛОБАЛ ХУВЬСАГЧУУД
;;; ----------------------------------------------------------------------------

;; Current standard: "MNS" or "EN"
(setq *FA-STANDARD* "MNS")

;; System type: "CONV" (Conventional) or "ADDR" (Addressable)
(setq *FA-SYSTEM-TYPE* "CONV")

;; Device counter for BOM
(setq *FA-DEVICE-LIST* nil)

;; Cable list for BOM
(setq *FA-CABLE-LIST* nil)

;; Zone counter
(setq *FA-ZONE-COUNT* 0)

;; Dynamic Block name
(setq *FA-BLOCK-NAME* "Fire devices")

;; Block source file path - CHANGE THIS TO YOUR PATH
(setq *FA-BLOCK-FILE* "C:/FireAlarmPlugin/blocks/Fire_devices.dwg")

;;; ----------------------------------------------------------------------------
;;; VISIBILITY STATES - Таны блокийн visibility states
;;; ----------------------------------------------------------------------------

(setq *FA-VIS-HEAT* "Heat detector")
(setq *FA-VIS-SMOKE* "Smoke detector")
(setq *FA-VIS-LINE-RX* "Line detector(Receiver)")
(setq *FA-VIS-LINE-TX* "Line detector(Transmitter)")
(setq *FA-VIS-SIREN* "Siren")
(setq *FA-VIS-MCP* "MCP")

;;; ----------------------------------------------------------------------------
;;; COVERAGE STANDARDS - ХАМРАХ ХҮРЭЭНИЙ СТАНДАРТУУД
;;; ----------------------------------------------------------------------------

;; MNS Standard (Монгол стандарт)
(setq *MNS-SMOKE-AREA* 85.0)      ; м²
(setq *MNS-SMOKE-RADIUS* 5.2)    ; м
(setq *MNS-HEAT-A1-AREA* 25.0)   ; м²
(setq *MNS-HEAT-A1-RADIUS* 2.8)  ; м
(setq *MNS-HEAT-A2-AREA* 35.0)   ; м²
(setq *MNS-HEAT-A2-RADIUS* 3.3)  ; м
(setq *MNS-HEAT-B-AREA* 50.0)    ; м²
(setq *MNS-HEAT-B-RADIUS* 4.0)   ; м

;; EN 54 Standard (Европын стандарт)
(setq *EN-SMOKE-AREA* 80.0)      ; м²
(setq *EN-SMOKE-RADIUS* 5.0)     ; м
(setq *EN-HEAT-A1-AREA* 30.0)    ; м²
(setq *EN-HEAT-A1-RADIUS* 3.1)   ; м
(setq *EN-HEAT-A2-AREA* 40.0)    ; м²
(setq *EN-HEAT-A2-RADIUS* 3.6)   ; м
(setq *EN-HEAT-B-AREA* 50.0)     ; м²
(setq *EN-HEAT-B-RADIUS* 4.0)    ; м

;;; ----------------------------------------------------------------------------
;;; ACTIVEX INITIALIZATION - Шаардлагатай
;;; ----------------------------------------------------------------------------

(vl-load-com)

;;; ----------------------------------------------------------------------------
;;; UTILITY FUNCTIONS - ТУСЛАХ ФУНКЦУУД
;;; ----------------------------------------------------------------------------

;; Get coverage radius based on current standard and device type
(defun FA:get-coverage-radius (device-type / radius)
  (cond
    ((= *FA-STANDARD* "MNS")
      (cond
        ((= device-type "SMOKE") *MNS-SMOKE-RADIUS*)
        ((= device-type "HEAT-A1") *MNS-HEAT-A1-RADIUS*)
        ((= device-type "HEAT-A2") *MNS-HEAT-A2-RADIUS*)
        ((= device-type "HEAT-B") *MNS-HEAT-B-RADIUS*)
        ((= device-type "HEAT") *MNS-HEAT-A2-RADIUS*)  ; Default heat
        (T 5.0)
      )
    )
    ((= *FA-STANDARD* "EN")
      (cond
        ((= device-type "SMOKE") *EN-SMOKE-RADIUS*)
        ((= device-type "HEAT-A1") *EN-HEAT-A1-RADIUS*)
        ((= device-type "HEAT-A2") *EN-HEAT-A2-RADIUS*)
        ((= device-type "HEAT-B") *EN-HEAT-B-RADIUS*)
        ((= device-type "HEAT") *EN-HEAT-A2-RADIUS*)
        (T 5.0)
      )
    )
    (T 5.0)
  )
)

;; Add device to BOM list
(defun FA:add-to-bom (device-type device-name device-mark point zone / item)
  (setq item (list
    (cons "TYPE" device-type)
    (cons "NAME" device-name)
    (cons "MARK" device-mark)
    (cons "X" (car point))
    (cons "Y" (cadr point))
    (cons "ZONE" zone)
  ))
  (setq *FA-DEVICE-LIST* (append *FA-DEVICE-LIST* (list item)))
  (princ (strcat "\n[FA] Төхөөрөмж нэмэгдлээ: " device-name))
)

;; Add cable to cable list
(defun FA:add-cable (cable-type length / item)
  (setq item (list
    (cons "TYPE" cable-type)
    (cons "LENGTH" length)
  ))
  (setq *FA-CABLE-LIST* (append *FA-CABLE-LIST* (list item)))
)

;; Draw coverage circle
(defun FA:draw-coverage (center radius / old-layer)
  (setq old-layer (getvar "CLAYER"))
  (FA:make-layer "FA-COVERAGE" 8 "DASHDOT")
  (setvar "CLAYER" "FA-COVERAGE")
  (command "_.CIRCLE" center radius)
  (setvar "CLAYER" old-layer)
)

;; Create or get layer
(defun FA:make-layer (layer-name color linetype / layer)
  (if (not (tblsearch "LAYER" layer-name))
    (progn
      (command "_.LAYER" "_M" layer-name "_C" (itoa color) "" "")
      (if (tblsearch "LTYPE" linetype)
        (command "_.LAYER" "_L" linetype "" "")
      )
    )
  )
  layer-name
)

;;; ----------------------------------------------------------------------------
;;; DYNAMIC BLOCK FUNCTIONS - ДИНАМИК БЛОК ФУНКЦУУД
;;; ----------------------------------------------------------------------------

;; Check if block exists, if not - prompt to load
(defun FA:check-block (/ block-exists)
  (setq block-exists (tblsearch "BLOCK" *FA-BLOCK-NAME*))
  (if (not block-exists)
    (progn
      (princ (strcat "\n[FA] '" *FA-BLOCK-NAME* "' блок олдсонгүй."))
      (princ "\n[FA] INSERT командаар блокоо оруулна уу эсвэл FABLOCK командыг ажиллуулна уу.")
      nil
    )
    T
  )
)

;; Load block from external file
(defun c:FABLOCK (/ file)
  (setq file (getfiled "Fire devices блок файл сонгох" "" "dwg" 0))
  (if file
    (progn
      (setq *FA-BLOCK-FILE* file)
      (command "_.INSERT" file "0,0" 1 1 0)
      (command "_.ERASE" "_L" "")
      (princ (strcat "\n[FA] Блок ачаалагдлаа: " *FA-BLOCK-NAME*))
    )
    (princ "\n[FA] Файл сонгогдоогүй.")
  )
  (princ)
)

;; Insert dynamic block and set visibility state
(defun FA:insert-dynamic-block (point visibility-state layer-name / block-ref props prop)
  ;; Check if block exists
  (if (not (FA:check-block))
    (progn
      (princ "\n[FA] Блок олдсонгүй! FABLOCK командаар эхлээд блокоо ачаална уу.")
      (exit)
    )
  )
  
  ;; Set layer
  (FA:make-layer layer-name 7 "Continuous")
  (setvar "CLAYER" layer-name)
  
  ;; Insert block
  (command "_.INSERT" *FA-BLOCK-NAME* point 1 1 0)
  
  ;; Get the last inserted block reference
  (setq block-ref (vlax-ename->vla-object (entlast)))
  
  ;; Set visibility state
  (if (and block-ref (vlax-property-available-p block-ref 'IsDynamicBlock))
    (if (= (vla-get-IsDynamicBlock block-ref) :vlax-true)
      (progn
        (setq props (vlax-invoke block-ref 'GetDynamicBlockProperties))
        (foreach prop props
          (if (= (vla-get-PropertyName prop) "Visibility1")
            (progn
              (vla-put-Value prop visibility-state)
              (princ (strcat "\n[FA] Visibility: " visibility-state))
            )
          )
        )
      )
    )
  )
  
  block-ref
)

;;; ----------------------------------------------------------------------------
;;; LAYER DEFINITIONS - ДАВХАРГЫН ТОДОРХОЙЛОЛТ
;;; ----------------------------------------------------------------------------

(defun FA:setup-layers ()
  ;; Device layers
  (FA:make-layer "FA-SMOKE" 1 "Continuous")      ; Red - Утааны датчик
  (FA:make-layer "FA-HEAT" 3 "Continuous")       ; Green - Дулааны датчик
  (FA:make-layer "FA-MCP" 5 "Continuous")        ; Blue - Гар дохио
  (FA:make-layer "FA-SIREN" 4 "Continuous")      ; Cyan - Дуут дохио
  (FA:make-layer "FA-LINE" 6 "Continuous")       ; Magenta - Шугаман датчик
  (FA:make-layer "FA-PANEL" 7 "Continuous")      ; White - Панел
  (FA:make-layer "FA-MODULE" 8 "Continuous")     ; Gray - Модуль
  
  ;; Cable layers
  (FA:make-layer "FA-CABLE" 1 "DASHED")          ; Red dashed - Кабель
  
  ;; Coverage layers
  (FA:make-layer "FA-COVERAGE" 8 "DASHDOT")      ; Gray - Хамрах хүрээ
  
  ;; Zone layers
  (FA:make-layer "FA-ZONE" 5 "PHANTOM")          ; Blue - Бүс
  
  (princ "\n[FA] Давхаргууд үүсгэгдлээ.")
)

;;; ----------------------------------------------------------------------------
;;; STANDARD SELECTION - СТАНДАРТ СОНГОХ
;;; ----------------------------------------------------------------------------

(defun c:FAST () ; Fire Alarm Standard Toggle
  (initget "MNS EN")
  (setq choice (getkword "\nСтандарт сонгоно уу [MNS/EN] <MNS>: "))
  (if (null choice) (setq choice "MNS"))
  (setq *FA-STANDARD* choice)
  (princ (strcat "\n[FA] Стандарт сонгогдлоо: " *FA-STANDARD*))
  (princ)
)

;;; ----------------------------------------------------------------------------
;;; SYSTEM TYPE SELECTION - СИСТЕМИЙН ТӨРӨЛ СОНГОХ
;;; ----------------------------------------------------------------------------

(defun c:FASYS () ; Fire Alarm System Type
  (initget "CONV ADDR")
  (setq choice (getkword "\nСистемийн төрөл [CONV/ADDR] <CONV>: "))
  (if (null choice) (setq choice "CONV"))
  (setq *FA-SYSTEM-TYPE* choice)
  (cond
    ((= choice "CONV") (princ "\n[FA] Conventional (Бүсчилсэн) систем сонгогдлоо"))
    ((= choice "ADDR") (princ "\n[FA] Addressable (Хаягжсан) систем сонгогдлоо"))
  )
  (princ)
)

;;; ----------------------------------------------------------------------------
;;; DEVICE PLACEMENT COMMANDS - ТӨХӨӨРӨМЖ БАЙРЛУУЛАХ КОМАНДУУД
;;; ----------------------------------------------------------------------------

;; FDS - Smoke Detector / Утааны датчик
(defun c:FDS (/ pt radius show-coverage)
  (FA:setup-layers)
  (setq radius (FA:get-coverage-radius "SMOKE"))
  
  (initget "Yes No")
  (setq show-coverage (getkword "\nХамрах хүрээ харуулах уу? [Yes/No] <Yes>: "))
  (if (null show-coverage) (setq show-coverage "Yes"))
  
  (while (setq pt (getpoint "\nУтааны датчикийн байрлал [Enter=дуусгах]: "))
    ;; Insert dynamic block with Smoke detector visibility
    (FA:insert-dynamic-block pt *FA-VIS-SMOKE* "FA-SMOKE")
    
    ;; Draw coverage if requested
    (if (= show-coverage "Yes")
      (FA:draw-coverage pt radius)
    )
    
    ;; Add to BOM
    (FA:add-to-bom "SMOKE" "Утааны мэдрэгч" "ИП-212" pt *FA-ZONE-COUNT*)
  )
  (princ)
)

;; FDH - Heat Detector / Дулааны датчик
(defun c:FDH (/ pt radius show-coverage heat-type)
  (FA:setup-layers)
  
  (initget "A1 A2 B")
  (setq heat-type (getkword "\nДулааны датчикийн төрөл [A1/A2/B] <A2>: "))
  (if (null heat-type) (setq heat-type "A2"))
  (setq radius (FA:get-coverage-radius (strcat "HEAT-" heat-type)))
  
  (initget "Yes No")
  (setq show-coverage (getkword "\nХамрах хүрээ харуулах уу? [Yes/No] <Yes>: "))
  (if (null show-coverage) (setq show-coverage "Yes"))
  
  (while (setq pt (getpoint "\nДулааны датчикийн байрлал [Enter=дуусгах]: "))
    ;; Insert dynamic block with Heat detector visibility
    (FA:insert-dynamic-block pt *FA-VIS-HEAT* "FA-HEAT")
    
    ;; Draw coverage if requested
    (if (= show-coverage "Yes")
      (FA:draw-coverage pt radius)
    )
    
    ;; Add to BOM
    (FA:add-to-bom (strcat "HEAT-" heat-type) 
                   (strcat "Дулааны мэдрэгч " heat-type) 
                   (strcat "ИП-101-" heat-type) 
                   pt 
                   *FA-ZONE-COUNT*)
  )
  (princ)
)

;; FMC - Manual Call Point / Гар мэдээллэгч
(defun c:FMC (/ pt)
  (FA:setup-layers)
  
  (while (setq pt (getpoint "\nГар мэдээллэгчийн байрлал [Enter=дуусгах]: "))
    ;; Insert dynamic block with MCP visibility
    (FA:insert-dynamic-block pt *FA-VIS-MCP* "FA-MCP")
    
    ;; Add to BOM
    (FA:add-to-bom "MCP" "Гар мэдээллэгч" "ИПР-513" pt *FA-ZONE-COUNT*)
  )
  (princ)
)

;; FDB - Siren / Дуут дохио өгөгч
(defun c:FDB (/ pt)
  (FA:setup-layers)
  
  (while (setq pt (getpoint "\nДуут дохио өгөгчийн байрлал [Enter=дуусгах]: "))
    ;; Insert dynamic block with Siren visibility
    (FA:insert-dynamic-block pt *FA-VIS-SIREN* "FA-SIREN")
    
    ;; Add to BOM
    (FA:add-to-bom "SIREN" "Дуут дохио өгөгч" "Маяк-12" pt *FA-ZONE-COUNT*)
  )
  (princ)
)

;; FDL - Line Detector / Шугаман мэдрэгч
(defun c:FDL (/ pt line-type)
  (FA:setup-layers)
  
  (initget "R T")
  (setq line-type (getkword "\nШугаман мэдрэгчийн төрөл [R=Receiver/T=Transmitter] <R>: "))
  (if (null line-type) (setq line-type "R"))
  
  (while (setq pt (getpoint "\nШугаман мэдрэгчийн байрлал [Enter=дуусгах]: "))
    ;; Insert dynamic block with appropriate visibility
    (if (= line-type "R")
      (progn
        (FA:insert-dynamic-block pt *FA-VIS-LINE-RX* "FA-LINE")
        (FA:add-to-bom "LINE-RX" "Шугаман мэдрэгч (хүлээн авагч)" "ИПДЛ-52" pt *FA-ZONE-COUNT*)
      )
      (progn
        (FA:insert-dynamic-block pt *FA-VIS-LINE-TX* "FA-LINE")
        (FA:add-to-bom "LINE-TX" "Шугаман мэдрэгч (дамжуулагч)" "ИПДЛ-52" pt *FA-ZONE-COUNT*)
      )
    )
  )
  (princ)
)

;;; ----------------------------------------------------------------------------
;;; CABLE ROUTING - КАБЕЛИЙН ТРАСС
;;; ----------------------------------------------------------------------------

;; FCR - Cable Route / Кабелийн трасс
(defun c:FCR (/ pts pt cable-type total-length first-pt)
  (FA:setup-layers)
  (FA:make-layer "FA-CABLE" 1 "DASHED")
  (setvar "CLAYER" "FA-CABLE")
  
  (initget "FIRE FRLS SHIELDED")
  (setq cable-type (getkword "\nКабелийн төрөл [FIRE/FRLS/SHIELDED] <FRLS>: "))
  (if (null cable-type) (setq cable-type "FRLS"))
  
  (setq pts nil)
  (setq total-length 0)
  (setq first-pt nil)
  
  (princ "\nКабелийн цэгүүдийг сонгоно уу [Enter=дуусгах]:")
  
  (setq first-pt (getpoint "\nЭхний цэг: "))
  (if first-pt
    (progn
      (setq pts (list first-pt))
      (while (setq pt (getpoint first-pt "\nДараагийн цэг: "))
        (command "_.LINE" (car pts) pt "")
        (setq total-length (+ total-length (distance (car pts) pt)))
        (setq pts (cons pt pts))
        (setq first-pt pt)
      )
    )
  )
  
  ;; Add cable to list with 10% reserve
  (if (> total-length 0)
    (progn
      (setq total-length (* total-length 1.1)) ; 10% нөөц
      (FA:add-cable cable-type total-length)
      (princ (strcat "\n[FA] Кабель: " cable-type " - " (rtos total-length 2 2) "м (10% нөөцтэй)"))
    )
  )
  (princ)
)

;;; ----------------------------------------------------------------------------
;;; ZONE MANAGEMENT - БҮСИЙН УДИРДЛАГА
;;; ----------------------------------------------------------------------------

;; FZN - Create Zone / Бүс үүсгэх
(defun c:FZN (/ pts pt zone-name first-pt)
  (FA:setup-layers)
  (FA:make-layer "FA-ZONE" 5 "PHANTOM")
  (setvar "CLAYER" "FA-ZONE")
  
  (setq *FA-ZONE-COUNT* (1+ *FA-ZONE-COUNT*))
  (setq zone-name (getstring T (strcat "\nБүсийн нэр <Zone " (itoa *FA-ZONE-COUNT*) ">: ")))
  (if (= zone-name "") (setq zone-name (strcat "Zone " (itoa *FA-ZONE-COUNT*))))
  
  (setq pts nil)
  (princ "\nБүсийн хилийг тодорхойлно уу [Enter=дуусгах]:")
  
  (while (setq pt (getpoint "\nЦэг сонгоно уу: "))
    (setq pts (cons pt pts))
  )
  
  (if (>= (length pts) 3)
    (progn
      ;; Draw zone boundary
      (command "_.PLINE")
      (foreach p (reverse pts) (command p))
      (command "_C")
      
      ;; Add zone label
      (setq center (FA:get-centroid pts))
      (command "_.TEXT" "_J" "_M" center 0.5 0 zone-name)
      
      (princ (strcat "\n[FA] Бүс үүсгэгдлээ: " zone-name))
    )
    (princ "\n[FA] Алдаа: Хамгийн багадаа 3 цэг шаардлагатай!")
  )
  (princ)
)

;; Helper: Get centroid of points
(defun FA:get-centroid (pts / sum-x sum-y n)
  (setq sum-x 0 sum-y 0 n (length pts))
  (foreach p pts
    (setq sum-x (+ sum-x (car p)))
    (setq sum-y (+ sum-y (cadr p)))
  )
  (list (/ sum-x n) (/ sum-y n))
)

;;; ----------------------------------------------------------------------------
;;; BOM / MATERIAL LIST - МАТ ТҮҮВЭР
;;; ----------------------------------------------------------------------------

;; Count devices by type
(defun FA:count-devices (device-list / counts item key existing)
  (setq counts nil)
  (foreach item device-list
    (setq key (cdr (assoc "TYPE" item)))
    (setq existing (assoc key counts))
    (if existing
      (setq counts (subst (cons key (1+ (cdr existing))) existing counts))
      (setq counts (cons (cons key 1) counts))
    )
  )
  ;; Convert to full item info
  (mapcar
    '(lambda (c)
      (list
        (car c)  ; type
        (cdr c)  ; count
        (FA:get-device-info (car c))
      )
    )
    counts
  )
)

;; Get device info for BOM
(defun FA:get-device-info (device-type)
  (cond
    ((= device-type "SMOKE") '("Утааны мэдрэгч" "ИП-212" "ш" "Таазанд" "S"))
    ((wcmatch device-type "HEAT*") '("Дулааны мэдрэгч" "ИП-101" "ш" "Таазанд" "H"))
    ((= device-type "MCP") '("Гар мэдээллэгч" "ИПР-513" "ш" "h=1.5м" "F"))
    ((= device-type "SIREN") '("Дуут дохио өгөгч" "Маяк-12" "ш" "h=2.3м" "◀"))
    ((= device-type "LINE-RX") '("Шугаман мэдрэгч (R)" "ИПДЛ-52" "ш" "" "L-R"))
    ((= device-type "LINE-TX") '("Шугаман мэдрэгч (T)" "ИПДЛ-52" "ш" "" "L-T"))
    ((= device-type "PANEL") '("Хяналтын панел" "Сигнал-20" "ш" "h=1.6м" "▣"))
    ((= device-type "MODULE") '("Модуль" "С2000-АР1" "ш" "" "▢"))
    (T '("Тодорхойгүй" "-" "ш" "" "?"))
  )
)

;; FBOM - Generate BOM Table / Мат түүвэр хүснэгт
(defun c:FBOM (/ pt row-height col-widths item-counts total-width current-y row-num)
  (if (null *FA-DEVICE-LIST*)
    (progn
      (princ "\n[FA] Алдаа: Төхөөрөмж байрлуулаагүй байна!")
      (exit)
    )
  )
  
  (setq pt (getpoint "\nХүснэгтийн байрлал: "))
  (if (null pt) (exit))
  
  (setq row-height 0.8)
  (setq col-widths '(1.0 8.0 4.0 2.5 2.0 6.0 2.0)) ; №, Нэр, Марк, Тоо, Нэгж, Тайлбар, Тэмдэг
  (setq total-width (apply '+ col-widths))
  
  ;; Group and count devices
  (setq item-counts (FA:count-devices *FA-DEVICE-LIST*))
  
  ;; Draw title
  (command "_.TEXT" "_J" "_M" 
    (list (+ (car pt) (/ total-width 2)) (+ (cadr pt) row-height)) 
    0.5 0 
    "ГАЛЫН ДОХИОЛЛЫН ТОНОГ ТӨХӨӨРӨМЖИЙН ЖАГСААЛТ"
  )
  
  ;; Draw header
  (command "_.RECTANGLE" pt (list (+ (car pt) total-width) (- (cadr pt) row-height)))
  
  (setq headers '("№" "Нэр" "Марк" "Тоо" "Нэгж" "Тайлбар" "Тэмдэг"))
  (setq curr-x (car pt))
  (setq idx 0)
  (foreach w col-widths
    (command "_.LINE" (list curr-x (cadr pt)) (list curr-x (- (cadr pt) row-height)) "")
    (command "_.TEXT" "_J" "_M" 
      (list (+ curr-x (/ w 2)) (- (cadr pt) (/ row-height 2))) 
      0.25 0 
      (nth idx headers)
    )
    (setq curr-x (+ curr-x w))
    (setq idx (1+ idx))
  )
  (command "_.LINE" (list curr-x (cadr pt)) (list curr-x (- (cadr pt) row-height)) "")
  
  ;; Draw data rows
  (setq row-num 1)
  (setq current-y (- (cadr pt) (* 2 row-height)))
  
  (foreach item item-counts
    (setq info (caddr item))
    (setq data (list
      (itoa row-num)
      (car info)
      (cadr info)
      (itoa (cadr item))
      (caddr info)
      (nth 3 info)
      (nth 4 info)
    ))
    
    ;; Row rectangle
    (command "_.RECTANGLE" 
      (list (car pt) (+ current-y row-height))
      (list (+ (car pt) total-width) current-y)
    )
    
    ;; Row data
    (setq curr-x (car pt))
    (setq idx 0)
    (foreach w col-widths
      (command "_.LINE" 
        (list curr-x (+ current-y row-height)) 
        (list curr-x current-y) 
        ""
      )
      (command "_.TEXT" "_J" "_M" 
        (list (+ curr-x (/ w 2)) (+ current-y (/ row-height 2))) 
        0.2 0 
        (nth idx data)
      )
      (setq curr-x (+ curr-x w))
      (setq idx (1+ idx))
    )
    
    (setq row-num (1+ row-num))
    (setq current-y (- current-y row-height))
  )
  
  ;; Add cable section if exists
  (if *FA-CABLE-LIST*
    (progn
      (setq current-y (- current-y row-height))
      (command "_.TEXT" "_J" "_M" 
        (list (+ (car pt) (/ total-width 2)) current-y) 
        0.3 0 
        "КАБЕЛЬ"
      )
      (setq row-num 1)
      (setq current-y (- current-y row-height))
      (foreach c *FA-CABLE-LIST*
        (command "_.TEXT" "_J" "_L" 
          (list (+ (car pt) 0.2) current-y) 
          0.2 0 
          (strcat (itoa row-num) ". " 
            (cond
              ((= (cdr (assoc "TYPE" c)) "FRLS") "FRLS 2x0.75")
              ((= (cdr (assoc "TYPE" c)) "FIRE") "КПСнг(А)-FRLS 2x0.75")
              ((= (cdr (assoc "TYPE" c)) "SHIELDED") "КПСЭнг(А)-FRLS 2x0.75")
              (T (cdr (assoc "TYPE" c)))
            )
            " - " (rtos (cdr (assoc "LENGTH" c)) 2 1) "м"
          )
        )
        (setq row-num (1+ row-num))
        (setq current-y (- current-y row-height))
      )
    )
  )
  
  (princ (strcat "\n[FA] Мат түүвэр хүснэгт үүсгэгдлээ. Нийт " (itoa (length item-counts)) " төрлийн төхөөрөмж."))
  (princ)
)

;;; ----------------------------------------------------------------------------
;;; EXCEL EXPORT - EXCEL ЭКСПОРТ
;;; ----------------------------------------------------------------------------

;; FEXP - Export to CSV (can be opened in Excel)
(defun c:FEXP (/ filename fp item-counts info row-num)
  (if (null *FA-DEVICE-LIST*)
    (progn
      (princ "\n[FA] Алдаа: Төхөөрөмж байрлуулаагүй байна!")
      (exit)
    )
  )
  
  (setq filename (getfiled "Excel файл хадгалах" "" "csv" 1))
  (if (null filename) (exit))
  
  (setq fp (open filename "w"))
  
  ;; Write BOM for UTF-8
  (write-line "№,Нэр,Марк,Тоо ширхэг,Нэгж,Тайлбар,Таних тэмдэг" fp)
  
  ;; Count devices
  (setq item-counts (FA:count-devices *FA-DEVICE-LIST*))
  
  ;; Data rows
  (setq row-num 1)
  (foreach item item-counts
    (setq info (caddr item))
    (write-line 
      (strcat
        (itoa row-num) ","
        (car info) ","
        (cadr info) ","
        (itoa (cadr item)) ","
        (caddr info) ","
        (nth 3 info) ","
        (nth 4 info)
      )
      fp
    )
    (setq row-num (1+ row-num))
  )
  
  ;; Cable section
  (write-line "" fp)
  (write-line "КАБЕЛЬ" fp)
  (if *FA-CABLE-LIST*
    (progn
      (setq row-num 1)
      (foreach c *FA-CABLE-LIST*
        (write-line
          (strcat
            (itoa row-num) ","
            (cond
              ((= (cdr (assoc "TYPE" c)) "FRLS") "FRLS 2x0.75")
              ((= (cdr (assoc "TYPE" c)) "FIRE") "КПСнг(А)-FRLS 2x0.75")
              ((= (cdr (assoc "TYPE" c)) "SHIELDED") "КПСЭнг(А)-FRLS 2x0.75")
              (T (cdr (assoc "TYPE" c)))
            )
            ",,"
            (rtos (cdr (assoc "LENGTH" c)) 2 1)
            ",м,,"
          )
          fp
        )
        (setq row-num (1+ row-num))
      )
    )
  )
  
  (close fp)
  (princ (strcat "\n[FA] Excel файл хадгалагдлаа: " filename))
  (princ)
)

;;; ----------------------------------------------------------------------------
;;; HELP / MENU - ТУСЛАМЖ / ЦЭС
;;; ----------------------------------------------------------------------------

;; FA - Main menu / Үндсэн цэс
(defun c:FA ()
  (princ "\n")
  (princ "\n╔══════════════════════════════════════════════════════════════╗")
  (princ "\n║     ГАЛЫН ДОХИОЛЛЫН СИСТЕМ v2.0 - DYNAMIC BLOCK VERSION     ║")
  (princ "\n╠══════════════════════════════════════════════════════════════╣")
  (princ "\n║  ТОХИРГОО:                                                   ║")
  (princ "\n║    FAST    - Стандарт сонгох (MNS/EN)                        ║")
  (princ "\n║    FASYS   - Системийн төрөл (Conventional/Addressable)     ║")
  (princ "\n║    FABLOCK - Блок файл ачаалах                               ║")
  (princ "\n╠══════════════════════════════════════════════════════════════╣")
  (princ "\n║  ТӨХӨӨРӨМЖ БАЙРЛУУЛАХ:                                       ║")
  (princ "\n║    FDS   - Утааны мэдрэгч (Smoke detector)                   ║")
  (princ "\n║    FDH   - Дулааны мэдрэгч (Heat detector)                   ║")
  (princ "\n║    FMC   - Гар мэдээллэгч (Manual call point)                ║")
  (princ "\n║    FDB   - Дуут дохио өгөгч (Siren)                          ║")
  (princ "\n║    FDL   - Шугаман мэдрэгч (Line detector R/T)               ║")
  (princ "\n╠══════════════════════════════════════════════════════════════╣")
  (princ "\n║  КАБЕЛЬ & БҮС:                                               ║")
  (princ "\n║    FCR   - Кабелийн трасс (Cable route)                      ║")
  (princ "\n║    FZN   - Бүс үүсгэх (Zone)                                 ║")
  (princ "\n╠══════════════════════════════════════════════════════════════╣")
  (princ "\n║  ГАРАЛТ:                                                     ║")
  (princ "\n║    FBOM  - Мат түүвэр хүснэгт (BOM table)                    ║")
  (princ "\n║    FEXP  - Excel экспорт (CSV)                               ║")
  (princ "\n╚══════════════════════════════════════════════════════════════╝")
  (princ "\n")
  (princ (strcat "Блок: " *FA-BLOCK-NAME* " | Стандарт: " *FA-STANDARD* " | Систем: " *FA-SYSTEM-TYPE*))
  (princ "\n")
  (princ)
)

;;; ----------------------------------------------------------------------------
;;; CLEAR DATA - ӨГӨГДӨЛ ЦЭВЭРЛЭХ
;;; ----------------------------------------------------------------------------

(defun c:FACLEAR ()
  (setq *FA-DEVICE-LIST* nil)
  (setq *FA-CABLE-LIST* nil)
  (setq *FA-ZONE-COUNT* 0)
  (princ "\n[FA] Бүх өгөгдөл цэвэрлэгдлээ.")
  (princ)
)

;;; ----------------------------------------------------------------------------
;;; AUTO LOAD MESSAGE
;;; ----------------------------------------------------------------------------

(princ "\n")
(princ "\n[FA] ═══════════════════════════════════════════════════════════")
(princ "\n[FA]   ГАЛЫН ДОХИОЛЛЫН PLUGIN v2.0 АЧААЛАГДЛАА")
(princ "\n[FA]   Dynamic Block: 'Fire devices'")
(princ "\n[FA]   ")
(princ "\n[FA]   Эхлэхийн өмнө:")
(princ "\n[FA]   1. FABLOCK командаар блок файлаа ачаална уу")
(princ "\n[FA]   2. FA командаар тусламж авна уу")
(princ "\n[FA] ═══════════════════════════════════════════════════════════")
(princ "\n")

(princ)
