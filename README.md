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

Cuenta con 2 módulos:

- Haskinator.
- Oraculo.

### Haskinator

Es el archivo principal del programa, cuenta con un main bastante simple que aplica la función menu que la podriamos ver como un while(condicion). Donde la condicion es que el valor ingresado por el usuario sea diferente a la opcion de salida (6). El metodo menu, por medio de un _case of_ aplica la función correspondiente a la opcion provista por el usuario (crear, cargar, predecir, etc...).

Cada función realizada por el programa se encuentra en este módulo, podríamos decir que este se encuentra dividido en 3 partes principales:

- Interacciones con el menu.
- Operaciones de entrada y salida.
- Manejo de predicciones y pregunta crucial.

En principio, la intención era crear dos módulos adicionales: uno para operaciones de entrada y salida, y uno para manejo de predicciones y pregunta crucial. A solicitud de los profesores, todos los metodos fueron incluidos acá en lugar de crear más módulos.

Cabe destacar que cada función cuenta con un encabezado de comentario que indica la aridad de la función y una breve descripción de su responsabilidad.

### Oráculo

Es el módulo encargado de definir los tipos de Oraculos, además de ofrecer una serie de metodos para facilitar su manipulación. Cada método sigue con exactitud las indicaciones del enunciado.

## Autores ✒️

- **Denylson Romero 13-11270** - [DenylR](https://github.com/DenylR)
- **Daniel Marin 10-10419** - [danmt](https://github.com/danmt)
