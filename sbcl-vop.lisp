(in-package :storage)

(defconstant +ascii-mask+
  #+x86-64 #xFFFFFF80FFFFFF80
  #+x86 #xFFFFFF80)

(defconstant +default-operand-size+ #+x86-64 :qword #+x86 :dword)

(define-vop (ascii-test-vop)
  (:policy :fast-safe)
  (:translate ascii-test)
  (:args (string :scs (sb-vm::descriptor-reg)))
  (:temporary (:sc sb-vm::unsigned-reg) end)
  (:temporary (:sc sb-vm::unsigned-reg) mask)
  (:results (res :scs (sb-vm::descriptor-reg)))
  (:generator 2
              (let ((loop (gen-label))
                    (true-exit (gen-label))
                    (false-exit (gen-label)))
                (inst mov end (sb-vm::make-ea +default-operand-size+
                                              :base string
                                              :disp (- (1- sb-vm:n-word-bytes))))
                #+x86-64(inst shl end 1)
                (inst inc string)
                (inst add end string)
                (inst mov mask +ascii-mask+)
                (emit-label loop)
                (inst test mask (sb-vm::make-ea +default-operand-size+ :base string))
                (inst jmp :ne false-exit)
                (inst add string sb-vm:n-word-bytes)
                (inst cmp string end)
                (inst jmp :b loop)
                (inst mov res (+ sb-vm::nil-value (sb-vm::static-symbol-offset t)))
                (inst jmp true-exit)
                (emit-label false-exit)
                (inst mov res sb-vm::nil-value)
                (emit-label true-exit))))
