type Matricula = Integer
type Nome = String
type Notas = (Double, Double, Double)
type Estudante = (Matricula, Nome, Notas)
type Turma = [Estudante]

exemplo :: Turma
exemplo = [(1, "Pedro", (5.0,7.5,4.5))
            ,(2, "Maria", (9.0,8.0,10.0))
            ,(3, "Joao", (3.0,7.0,4.5))
            ,(4, "Paulo", (7.0,5.0,9.5))
            ,(5, "Ana", (8.5,8.0,9.0))
            ]

--resposta a)
somaMedias :: Turma -> Double
somaMedias [] = 0
somaMedias ((_, _, (n1, n2, n3)) : xs) = (n1, n2, n3) / 3 + somaMedias xs

mediaTurma :: Turma -> Double
mediaTurma t = somaMedias t / fromIntegral (length t)
