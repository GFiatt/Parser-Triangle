# Parser Triangle
Este proyecto es un parser de un compilador para el lenguaje Triangle. Este genera árboles de parsing a partir de una serie de Tokens dados. A continuación se detallan los pasos para compilar y ejecutar el proyecto.

## Requisitos
Antes de ejecutar este proyecto, es necesario instalar Graphviz, que es una biblioteca para la generación de imágenes a partir de archivos .dot.

## Instrucciones para ejecutar el proyecto

### 1. Compilar el proyecto: 
Para compilar todos los archivos .rs en el proyecto, ejecuta:

```cargo build```

### 2. Ejecutar el tokenizador: 
Use el siguiente comando para ejecutar el tokenizador, donde `TestCases-Begin/BeginOK4.tri` es un archivo de prueba que se puede variar dependiendo de cuál archivo de prueba desee usar. Los archivos de prueba se encuentran en la carpeta Inputs, que contiene varias subcarpetas con diferentes archivos.

```cargo run --bin tokenize TestCases-Begin/BeginOK4.tri```

Este comando generará un archivo llamado tokens.out en la carpeta Outputs.

### 3. Ejecutar el parser: 
Para ejecutar el parser y generar el archivo tree.out, use el siguiente comando:

```cargo run --bin parse Outputs/tokens.out```

### 4. Generar el archivo .dot: 
Para generar el archivo .dot que representa el árbol de parsing, ejecute el siguiente comando:

```cargo run --bin pare -- tree.out -o tree.dot```

### 5. Generar la imagen del árbol sintáctico: 
Para convertir el archivo .dot en una imagen PNG del árbol, ejecute el siguiente comando:

```dot -Tpng Outputs/tree.dot -o tree.png```

Este comando generará la imagen tree.png, que contiene la representación gráfica del árbol.
