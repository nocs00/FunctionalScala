val projectPath = getClass.getResource("").getPath

val keyPath = "keys/bla"


val finalPath = if (keyPath.startsWith("/")) keyPath else projectPath + keyPath


