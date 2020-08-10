#Ejercicio optativo (20 puntos)

#Corra cada una de las líneas y explique qué realizan todas las funciones utilizadas en 
#el siguiente código. Puede apoyarse ingresando a la página y hacer click en 
#“Inspeccionar elemento”.

fullDatos <- data.frame()
for(numeroPagina in 1:5){
  readHtml <- read_html(paste("https://www.yapo.cl/region_metropolitana?ca=15_s&o=",numeroPagina,sep = ""))
  print(paste("Descargando pagina nro:",numeroPagina))
  nodeTabla <- html_nodes(readHtml, ".listing_thumbs")
  nodeTabla <- html_nodes(nodeTabla, ".title")
  linksProductos <- html_attr(nodeTabla,"href")
  
  for (urlYapo in linksProductos) {
    htmlSeg <- read_html(urlYapo)
    
    print(paste("Descargando URL ==> ",urlYapo))
    
    textoTipoAviso <- obtenerCategoria(htmlSeg)
    precio <- sacandoPrecio(htmlSeg)
    fullDatos <- rbind(fullDatos,data.frame(categoria = textoTipoAviso, precio = precio))
  }
}

View(fullDatos)

##Desarrollo

#En la linea 7 se crea la variable "fullDatos" la que guarda un data frames vacio. El 
#data frames es una estructura de datos similar a una hoja de datos tipo excel o csv.

fullDatos <- data.frame()

#En la linea 8 se crea una funcion for, con la variable "numeroPagina" que recorre el 
#sitio web de yapo filtrado por la region metropolitana desde la pagina 1 a la 5 

for(numeroPagina in 1:5)

#En la linea 9 se crea la variable "readHtml" que contiene la funcion read_html la que 
#permite leer el código HTML de cualquier pagina web, donde el argumento es la dirección 
#URL de la página yapo. El argumento de la funcion que este caso es el link de pagina
#esta concatenado con una funcion paste, que junta el link de la pagina con la variable
#numero de pagina creada en el for para recorrer, ademas de la funcion sep="" entendida
#como un separador de campo que en este caso es un espacio, debido a que esta entre comillas

readHtml <- read_html(paste("https://www.yapo.cl/region_metropolitana?ca=15_s&o=",numeroPagina,sep = ""))
  
#En la linea 10 se crea la funcion print que busca que se imprima en la consola los valores
#concatenados por la funcion precedente paste, que junta el texto escrito con la variable  
#numeroPagina que recorre el primer for creado en la funcion, por lo tanto lo que se imprime
#en la consola es "Descargando pagina nro: y el numero de la pagina a medida que va 
#recorriendo el for

print(paste("Descargando pagina nro:",numeroPagina))

#En la linea 11 y 12 se crea la variable "nodetable" la que contiene a la funcion html_nodes 
#que se encarga de la extracción de una parte específica del código HTML, que en este caso
#son las tablas que contienen la descripcion de los productos que son comercializados, teniendo
#como argumento el read html que lee el codigo y la clase ".listing_thumbs" correspondiente a la tabla
#luego se pisa la variable creada con la misma funcion pero ahora extrae el titulo del
#producto con la clase ".title"

nodeTabla <- html_nodes(readHtml, ".listing_thumbs")
nodeTabla <- html_nodes(nodeTabla, ".title")

#En la linea 13 se crea una nueva variable de nombre linksProductos, la cual mediante la funcion
#html_attr permite extraer y acceder a los atributos del código, teniendo como argumentos la variable
#creada en la linea precedente de nombre nodeTabla y "href" que en la pagina corresponde a los
#links desde donde provienen las tablas dentro de la url

linksProductos <- html_attr(nodeTabla,"href")

#En la linea 15 se crea una nueva funcion for, la que mediante "urlYapo" recorrera la variable
#creada en la linea anterior llamada linksProductos

for (urlYapo in linksProductos)

#En la linea 16 se crea la variable htmlSeg la que mediante la funcion read_html, como se 
#mencino anteriormente, permite leer el código HTML de la pagina web de yapo en este caso, 
#ademas como argumento se utiliza la variable creada en la funcion anterior correspondiente
#al for, llamada "urlYapo" que recorrio linksProductos

htmlSeg <- read_html(urlYapo)
  
#En la linea 18 se utiliza la funcion print para imprimir en la consola un paste, que concatena
#el texto "Descargando URL ==> "con urlYapo que es la variable que recorre el for

print(paste("Descargando URL ==> ",urlYapo))

#En la linea 20 se crea la variable "textoTipoAviso" que mediante la funcion creada en el 
#documento "libreriasYapo.R" llamada "obtenerCategoria" permite que al ingresar "htmlSeg" 
#(variable creada posterior al segundo for que lee el codigo html de urlYapo), devuelve o retorna 
#la variable nodoBraed dentro de una funcion html_text, la cual extrae atributos, texto y nombres 
#de etiqueta de html. En este caso especifico extrae piezas de documentos html utilizando selectores 
#xpath y css,asociados al diseño de la pagina y especificamente la clase breadcrumbs y el atributo
#strong, que corresponde a texto en negrita

textoTipoAviso <- obtenerCategoria(htmlSeg)

obtenerCategoria <- function(htmlSeg){
  nodoBread <- html_nodes(htmlSeg, ".breadcrumbs")
  nodoBread <- html_nodes(nodoBread, "strong")
  return(html_text(nodoBread))
}

#En la linea 21 se crea la variable precio, donde se almacena la funcion sacandoPrecio, creada en
#el documento "libreriasYapo.R" la cual permite que al ingresar la variable htmlSeg devuelva
#el precio obtenido, referente a la funcion sacando precio se puede decir que contiene a la 
#funcion html_nodes (que permite extracción de partes específicas del código HTML, utilizando
#un selector llamado ".offer" que extrae el precio desde la tabla en la pagina de yapo, y que
#lo guarda en una variable llamada nodoBread, posterior a esto utilizando la funcion condicional
#if poniendo como condicional que si el largo de nodobread es mayor que 0 cree una funcion precio
# con la variable nodobread y en caso contrario devuelva el valor NA. Con respecto a la variable
#creada dentro del if de nombre precio se le aplica una funcion gsub que se usa para limpiar los 
#datos poniendo como primer termino el valor a reemplazar por el valor definitivo y asi
#se va pisando la variable eliminado los valores \\t, \\n, \\$, [.], y reemplazando por un 
#espacio vacio representado por "" (nada),y como ultimo valor del argumneto donde lo va a hacer (precio)}
#finalmente con la funcion as.numeric se asegura que el valor devuelto es un numero y por ultimo esta 
#la funcion return la que devuelve o retorna el precio

precio <- sacandoPrecio(htmlSeg)

sacandoPrecio <- function(htmlSeg){
  nodoBread <- html_nodes(htmlSeg, ".offer")
  if(length(nodoBread)>0){
    precio <- html_text(nodoBread)
    precio <- gsub("\\t","",precio)
    precio <- gsub("\\n","",precio)
    precio <- gsub("\\$","",precio)
    precio <- gsub("[.]","",precio)
    precio <- as.numeric(precio)
  }else{
    precio = NA
  }
  return(precio)
}

#En la linea 22 se le agrega la funcion rbind a la variable creada al comienzo de nombre 
#fullDatos la que permite tomar una secuencia de dataframes y combinar por columnas o filas, 
#mediante los argumentps, en el ejemplo la primera columna seria categoria correspondiente
#al textoTipoAviso y la segunda el precio con el mismo nombre

fullDatos <- rbind(fullDatos,data.frame(categoria = textoTipoAviso, precio = precio))

#Finalmente en la linea 23 y 24, se cierra con los corchetes de los 2 for creados anteriormente


##Por ultimo a modo de resumen, se detallan las funciones utilizadas con su respectivo uso:

#paste: concatena codigos por ejemplo en este caso un texto con una variable

#print: imprime en la consola los valores a medida que va corriendo la funcion o al final de esta

#if: funcion condicional que en caso de ser verdadera o falsa realiza la gestion requerida dependiendo
#del valor logico

#for: recorre el vectores, listas, secuencias, etc, en esta caso recorre el sitio web de yapo 
#filtrado por la region metropolitana desde la pagina 1 a la 5 (primer for)

#retrurn: devuelve en la consola el resultado de una funcion

#sep="": es un separador de campo que en este caso es un espacio, debido a que esta entre comillas

#as.numeric: hace que el valor ingresado en los argumnetos sea numerico

#rbind: toma una secuencia de variables, vectores, listas, etc. en este caso dataframes y combina por 
#columnas o filas

#data.frame: El data frames es una estructura de datos similar a una hoja de datos tipo excel o 
#csv que sirve para guardar datos en forma ordenada de tipo columnas y filas.

#gsub: se usa para limpiar los datos extraidos de una pagina web, poniendo como primer termino el 
#valor a reemplazar por el valor definitivo y al final el argumento desde donde lo va a hacer (reemplaza)

#read_html: permite leer el código HTML de cualquier pagina web, donde el argumento es la 
#dirección (lee como pagina web)

#html_text: extrae atributos, texto y nombres de una pagina web utilizando por ejemplo css que 
#corresponde al diseño de la pagina (guarda datos de la pagina)

#html_nodes: extrae una parte específica del código HTML como por ejemplo tablas 

#html_attr: permite extraer y acceder a los atributos del código

#obtenerCategoria:funcion creada en libreriasYapo.R permite que al ingresar "htmlSeg" devuelva
#la variable nodoBraed

#sacandoPrecio: funcion creada en libreriasYapo.R la cual permite que al ingresar la variable htmlSeg 
#devuelva el precio obtenido



#Juan Acuña Vega, Curso Big data, ICI, Utem