# Haskinator

En las profundidades del bosque de los mil y un Monads, en la apartada y escondida cuna del gran río Curry,
vive en su choza de piedra el poderoso oráculo Haskinator. Con su vasto conocimiento, Haskinator es capaz de
adivinar los pensamientos de aquellos que lo visitan, sin más que unas pocas y puntuales preguntas. El gran oráculo
se ha hecho conocido y afamado por su gran poder, sorprendiendo y maravillando a cada viajero con la suerte de
presenciarlo.

Sin embargo, Haskinator ahora siente que sus poderes se desvanecen en el tiempo y teme defraudar a quienes
expectantes desean ser maravillados por él. Velozmente maquina una forma de conservar el don que se le escapa
cuando recuerda la historia de uno de sus tantos visitantes. El visitante le había contado sobre un grupo de talentosos
estudiantes que aprendían a programar en Haskell, el lenguaje de los antiguos dioses y oráculos de eras pasadas.

Usando el poco poder telepático que le quedaba, el gran Haskinator se comunica con los estudiantes y les
encomienda la creación de un programa que logre emular sus legendarias capacidades de predicción.

## Comenzando 🚀

Estas instrucciones te permitirán obtener una copia del proyecto en funcionamiento en tu máquina local.

- Clona el repositorio
- Ejecuta `make` para correr el Makefile
- Ejecuta `./Haskinator` para correr el programa

## Construido con 🛠️

- [Haskell](https://www.haskell.org/) - El lenguaje usado

## Diseño 🧠

El programa fue diseñado teniendo en cuenta las prácticas de la programación funcional. La prioridad fue hacer un programa declarativo que con pura composición de funciones fuese capaz de realizar las predicciones.

Cuenta con 4 módulos:

- Haskinator.
- Oraculo.
- Predicciones.
- IO Helper.

### Haskinator

Es el archivo principal del programa, cuenta con un main bastante simple que aplica la función menu que la podriamos ver como un while(condicion). Donde la condicion es que el valor ingresado por el usuario sea diferente a la opcion de salida (6). El metodo menu, por medio de un _case of_ aplica la función correspondiente a la opcion provista por el usuario (crear, cargar, predecir, etc...).

### Oraculo

Es el módulo encargado de definir los tipos de Oraculos, además de ofrecer una serie de metodos para facilitar su manipulación.

### Predicciones

El módulo de predicciones es un modulo adicional, creado con la intencion de encapsular todos los metodos relacionados a la predicción. Realizar una predicción es algo complejo y que no tiene relacion directa con el main, por lo que nos parecio una brillante idea mover toda la _logica de negocio_ a un archivo separado.

### IO Helper

Para limitar las interacciones con el mecanismo IO de Haskell, creamos un modulo responsable de hacer todas los read/write del sistema, de esta manera pudimos tener una implementacion mas limpia y clara en el resto de los modulos.

Cabe destacar que cada función cuenta con un encabezado de comentario que indica la aridad de la función y una breve descripción de su responsabilidad.

## Autores ✒️

- **Denylson Romero 13-11270** - [DenylR](https://github.com/DenylR)
- **Daniel Marin 10-10419** - [danmt](https://github.com/danmt)
