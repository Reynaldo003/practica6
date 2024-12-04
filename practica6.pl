cuadrado(S, A):-A is S ** 2.

rectangulo(B, H, A):-A is B * H.

paralelogramo(B, H, A):-A is B * H.

trapezoide(A, B, H, Ar):-Ar is ((A + B) / 2) * H.

triangulo(B, H, A):-A is (1/2) * B * H.

elipse(A, B, Ar):-Ar is pi * A * B.

circulo(R, A):-A is pi * R ** 2.

formula_general(A, B, C, X1, X2) :-
    Discriminante is B * B - 4 * A * C,  
    Discriminante >= 0,                   
    Raiz_discriminante is sqrt(Discriminante),  
    X1 is (-B + Raiz_discriminante) / (2 * A),  
    X2 is (-B - Raiz_discriminante) / (2 * A).  


en_base(N, Base) :-
    N >= 0,                 
    Base >= 2,              
    en_base_aux(N, Base).

en_base_aux(0, _).          
en_base_aux(N, Base) :- 
    N < Base,                  
    N >= 0.                    
en_base_aux(N, Base) :-
    N >= Base,                 
    Restante is N // Base,     
    en_base_aux(Restante, Base).  

base_convierte(0, _, [0]) :- !.
base_convierte(Numero, Base, Resultado) :-
    Numero > 0,
    Div is Numero // Base,
    Resto is Numero mod Base,
    base_convierte(Div, Base, DigitosParciales),
    append(DigitosParciales, [Resto], Resultado).

convertir(Numero, Base, Resultado) :-
    base_convierte(Numero, Base, Digitos),
    atomic_list_concat(Digitos, '', ResultadoAtomico),
    atom_number(ResultadoAtomico, Resultado).

mayor(Numero, DigitoMayor) :-
    atom_chars(Numero, DigitosAtomicos),
    maplist(atom_number, DigitosAtomicos, Digitos),
    max_list(Digitos, DigitoMayor).

contar(Numero, Digito, N) :-
    atom_chars(Numero, DigitosAtomicos), 
    maplist(atom_number, DigitosAtomicos, Digitos), 
    include(=(Digito), Digitos, DigitosFiltrados), 
    length(DigitosFiltrados, N). 

posicion(Num, Dig, Pos) :-
    atom_chars(Num, DigitosAtomicos),       
    maplist(atom_number, DigitosAtomicos, Digitos), 
    reverse(Digitos, DigitosReverso),         
    nth1(PosicionInversa, DigitosReverso, Dig), 
    length(Digitos, Longitud),                
    Pos is Longitud - PosicionInversa + 1, !. 
posicion(_, _, 0).                     

rot_izq(Num, N, Res) :-
    atom_chars(Num, DigitosAtomicos),       
    maplist(atom_number, DigitosAtomicos, Digitos), 
    length(Digitos, Longitud),                
    R is N mod Longitud,                      
    append(Inicio, Final, Digitos),           
    length(Inicio, R),                        
    append(Final, Inicio, DigitosRotados),   
    atomic_list_concat(DigitosRotados, '', ResultadoAtomico), 
    atom_number(ResultadoAtomico, Res). 
