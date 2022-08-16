structure Array =
struct
  open Array

  fun alloc (n : int) : 'a array = Unsafe.cast (array (n, 0))
end

structure Word8Array =
struct
  open Word8Array

  fun alloc (n : int) : array = array (n, Word8.fromInt 0)
end
