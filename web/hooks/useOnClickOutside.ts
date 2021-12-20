import { Ref, useEffect, useRef } from 'react'

type AnyEvent = MouseEvent | TouchEvent
type Element = HTMLDivElement | HTMLUListElement

export default function useOnClickOutside<T extends Element>(
  handler: (event: AnyEvent) => void
): Ref<T> {
  const ref = useRef<T>(null)

  useEffect(() => {
    const listener = (event: AnyEvent) => {
      const element = ref?.current

      if (!element || element.contains(event.target as Node)) {
        return
      }

      handler(event)
    }

    document.addEventListener('mousedown', listener)
    document.addEventListener('touchstart', listener)

    return () => {
      document.removeEventListener('mousedown', listener)
      document.removeEventListener('touchstart', listener)
    }
  }, [ref, handler])

  return ref
}
