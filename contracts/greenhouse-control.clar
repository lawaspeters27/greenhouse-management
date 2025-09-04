;; Greenhouse Operation Management System
;; A smart contract for managing greenhouse operations including climate control, crop scheduling, and yield optimization

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u1))
(define-constant ERR_GREENHOUSE_NOT_FOUND (err u2))
(define-constant ERR_INVALID_CROP_ID (err u3))
(define-constant ERR_INVALID_READINGS (err u4))
(define-constant ERR_INVALID_SCHEDULE (err u5))

;; Data Variables
(define-data-var next-greenhouse-id uint u1)
(define-data-var next-crop-id uint u1)

;; Data Maps

;; Greenhouse registry with climate control settings
(define-map greenhouses 
  uint 
  {
    owner: principal,
    name: (string-ascii 50),
    target-temp: uint,
    target-humidity: uint,
    lighting-hours: uint,
    status: (string-ascii 20)
  }
)

;; Real-time environmental readings
(define-map environmental-data
  {greenhouse-id: uint, timestamp: uint}
  {
    temperature: uint,
    humidity: uint,
    co2-level: uint,
    light-intensity: uint
  }
)

;; Crop management and scheduling
(define-map crops
  uint
  {
    greenhouse-id: uint,
    crop-type: (string-ascii 30),
    planted-at: uint,
    expected-harvest: uint,
    current-stage: (string-ascii 20),
    yield-target: uint
  }
)

;; Yield tracking and optimization
(define-map harvest-records
  {greenhouse-id: uint, crop-id: uint, harvest-date: uint}
  {
    actual-yield: uint,
    quality-score: uint,
    notes: (string-ascii 100)
  }
)

;; Access control for greenhouse operators
(define-map operators
  {greenhouse-id: uint, operator: principal}
  bool
)

;; Public Functions

;; Register a new greenhouse facility
(define-public (register-greenhouse (name (string-ascii 50)) (target-temp uint) (target-humidity uint) (lighting-hours uint))
  (let ((greenhouse-id (var-get next-greenhouse-id)))
    (map-set greenhouses greenhouse-id {
      owner: tx-sender,
      name: name,
      target-temp: target-temp,
      target-humidity: target-humidity,
      lighting-hours: lighting-hours,
      status: "active"
    })
    (var-set next-greenhouse-id (+ greenhouse-id u1))
    (ok greenhouse-id)
  )
)

;; Update greenhouse climate settings
(define-public (update-climate-settings (greenhouse-id uint) (target-temp uint) (target-humidity uint) (lighting-hours uint))
  (let ((greenhouse (map-get? greenhouses greenhouse-id)))
    (asserts! (is-some greenhouse) ERR_GREENHOUSE_NOT_FOUND)
    (asserts! (or (is-eq tx-sender (get owner (unwrap-panic greenhouse)))
                  (is-authorized-operator greenhouse-id tx-sender)) ERR_UNAUTHORIZED)
    (map-set greenhouses greenhouse-id (merge (unwrap-panic greenhouse) {
      target-temp: target-temp,
      target-humidity: target-humidity,
      lighting-hours: lighting-hours
    }))
    (ok true)
  )
)

;; Record environmental sensor readings
(define-public (record-environmental-data (greenhouse-id uint) (temperature uint) (humidity uint) (co2-level uint) (light-intensity uint))
  (let ((greenhouse (map-get? greenhouses greenhouse-id)))
    (asserts! (is-some greenhouse) ERR_GREENHOUSE_NOT_FOUND)
    (asserts! (or (is-eq tx-sender (get owner (unwrap-panic greenhouse)))
                  (is-authorized-operator greenhouse-id tx-sender)) ERR_UNAUTHORIZED)
    (asserts! (and (>= temperature u0) (<= temperature u50) 
                   (>= humidity u0) (<= humidity u100)
                   (>= co2-level u0) (<= co2-level u2000)
                   (>= light-intensity u0) (<= light-intensity u100)) ERR_INVALID_READINGS)
    (map-set environmental-data {greenhouse-id: greenhouse-id, timestamp: stacks-block-height} {
      temperature: temperature,
      humidity: humidity,
      co2-level: co2-level,
      light-intensity: light-intensity
    })
    (ok true)
  )
)

;; Schedule new crop planting
(define-public (schedule-crop (greenhouse-id uint) (crop-type (string-ascii 30)) (expected-harvest uint) (yield-target uint))
  (let ((greenhouse (map-get? greenhouses greenhouse-id))
        (crop-id (var-get next-crop-id)))
    (asserts! (is-some greenhouse) ERR_GREENHOUSE_NOT_FOUND)
    (asserts! (or (is-eq tx-sender (get owner (unwrap-panic greenhouse)))
                  (is-authorized-operator greenhouse-id tx-sender)) ERR_UNAUTHORIZED)
    (asserts! (> expected-harvest stacks-block-height) ERR_INVALID_SCHEDULE)
    (map-set crops crop-id {
      greenhouse-id: greenhouse-id,
      crop-type: crop-type,
      planted-at: stacks-block-height,
      expected-harvest: expected-harvest,
      current-stage: "planted",
      yield-target: yield-target
    })
    (var-set next-crop-id (+ crop-id u1))
    (ok crop-id)
  )
)

;; Update crop growth stage
(define-public (update-crop-stage (crop-id uint) (new-stage (string-ascii 20)))
  (let ((crop (map-get? crops crop-id)))
    (asserts! (is-some crop) ERR_INVALID_CROP_ID)
    (let ((greenhouse-id (get greenhouse-id (unwrap-panic crop))))
      (asserts! (or (is-authorized-for-greenhouse greenhouse-id tx-sender)
                    (is-authorized-operator greenhouse-id tx-sender)) ERR_UNAUTHORIZED)
      (map-set crops crop-id (merge (unwrap-panic crop) {
        current-stage: new-stage
      }))
      (ok true)
    )
  )
)

;; Record harvest yield and quality
(define-public (record-harvest (crop-id uint) (actual-yield uint) (quality-score uint) (notes (string-ascii 100)))
  (let ((crop (map-get? crops crop-id)))
    (asserts! (is-some crop) ERR_INVALID_CROP_ID)
    (let ((greenhouse-id (get greenhouse-id (unwrap-panic crop))))
      (asserts! (or (is-authorized-for-greenhouse greenhouse-id tx-sender)
                    (is-authorized-operator greenhouse-id tx-sender)) ERR_UNAUTHORIZED)
      (asserts! (<= quality-score u10) ERR_INVALID_READINGS)
      (map-set harvest-records {greenhouse-id: greenhouse-id, crop-id: crop-id, harvest-date: stacks-block-height} {
        actual-yield: actual-yield,
        quality-score: quality-score,
        notes: notes
      })
      (map-set crops crop-id (merge (unwrap-panic crop) {
        current-stage: "harvested"
      }))
      (ok true)
    )
  )
)

;; Add authorized operator for greenhouse
(define-public (add-operator (greenhouse-id uint) (operator principal))
  (let ((greenhouse (map-get? greenhouses greenhouse-id)))
    (asserts! (is-some greenhouse) ERR_GREENHOUSE_NOT_FOUND)
    (asserts! (is-eq tx-sender (get owner (unwrap-panic greenhouse))) ERR_UNAUTHORIZED)
    (map-set operators {greenhouse-id: greenhouse-id, operator: operator} true)
    (ok true)
  )
)

;; Remove authorized operator
(define-public (remove-operator (greenhouse-id uint) (operator principal))
  (let ((greenhouse (map-get? greenhouses greenhouse-id)))
    (asserts! (is-some greenhouse) ERR_GREENHOUSE_NOT_FOUND)
    (asserts! (is-eq tx-sender (get owner (unwrap-panic greenhouse))) ERR_UNAUTHORIZED)
    (map-delete operators {greenhouse-id: greenhouse-id, operator: operator})
    (ok true)
  )
)

;; Read-only Functions

;; Get greenhouse information
(define-read-only (get-greenhouse (greenhouse-id uint))
  (map-get? greenhouses greenhouse-id)
)

;; Get latest environmental data
(define-read-only (get-environmental-data (greenhouse-id uint) (timestamp uint))
  (map-get? environmental-data {greenhouse-id: greenhouse-id, timestamp: timestamp})
)

;; Get crop information
(define-read-only (get-crop (crop-id uint))
  (map-get? crops crop-id)
)

;; Get harvest record
(define-read-only (get-harvest-record (greenhouse-id uint) (crop-id uint) (harvest-date uint))
  (map-get? harvest-records {greenhouse-id: greenhouse-id, crop-id: crop-id, harvest-date: harvest-date})
)

;; Check if climate conditions are optimal
(define-read-only (check-climate-optimal (greenhouse-id uint) (current-temp uint) (current-humidity uint))
  (let ((greenhouse (map-get? greenhouses greenhouse-id)))
    (match greenhouse
      greenhouse-data
      (let ((temp-diff (if (>= current-temp (get target-temp greenhouse-data))
                         (- current-temp (get target-temp greenhouse-data))
                         (- (get target-temp greenhouse-data) current-temp)))
            (humidity-diff (if (>= current-humidity (get target-humidity greenhouse-data))
                             (- current-humidity (get target-humidity greenhouse-data))
                             (- (get target-humidity greenhouse-data) current-humidity))))
        (and (<= temp-diff u3) (<= humidity-diff u10))
      )
      false
    )
  )
)

;; Calculate yield efficiency percentage
(define-read-only (calculate-yield-efficiency (crop-id uint) (actual-yield uint))
  (let ((crop (map-get? crops crop-id)))
    (match crop
      crop-data
      (let ((target (get yield-target crop-data)))
        (if (> target u0)
          (/ (* actual-yield u100) target)
          u0
        )
      )
      u0
    )
  )
)

;; Private Functions

;; Check if sender is authorized for greenhouse operations
(define-private (is-authorized-for-greenhouse (greenhouse-id uint) (sender principal))
  (let ((greenhouse (map-get? greenhouses greenhouse-id)))
    (match greenhouse
      greenhouse-data
      (is-eq sender (get owner greenhouse-data))
      false
    )
  )
)

;; Check if sender is authorized operator
(define-private (is-authorized-operator (greenhouse-id uint) (operator principal))
  (default-to false (map-get? operators {greenhouse-id: greenhouse-id, operator: operator}))
)
