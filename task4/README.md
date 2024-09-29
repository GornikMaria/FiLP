# Вопросы

Вопрос 1: Что происходит, если вы пытаетесь вставить посредством функции myInsert новое значение для существующего ключа?
```cpp
myInsert :: Ord k => k -> a -> Map k a -> Map k a
myInsert key value EmptyMap = Node key value EmptyMap EmptyMap
myInsert key value (Node k v left right)
  | key == k  = Node k value left right  -- обновляем значение для существующего ключа
  | key < k   = Node k v (myInsert key value left) right  -- вставляем в левое поддерево
  | key > k   = Node k v left (myInsert key value right)  -- вставляем в правое поддерево
```

a) Новый узел добавляется в левое поддерево.
b) Новый узел заменяет существующий узел с таким же ключом.
c) Узел игнорируется, и дерево остается неизменным.
d) Возникает ошибка, и вставка не происходит.

Правильный ответ: b) Новый узел заменяет существующий узел с таким же ключом.

Вопрос 2: Какой из следующих вариантов кода правильно определяет функцию myTail?

a) 
myTail [ ] = [ ]
myTail (x:xs) = xs

b)
myTail [] = []
myTail (x:xs) = x

c) myTail xs = tail xs

d) myTail xs = xs

Правильный ответ: a) myTail [ ] = [ ]
                     myTail (x:xs) = xs


