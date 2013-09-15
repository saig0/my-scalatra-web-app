package de.le.app

import org.scalatra._
import scalate.ScalateSupport
import de.le.app.model._
import net.liftweb.json._
import net.liftweb.json.JsonDSL._

class MyServlet extends ScalatraServlet with ScalateSupport with JsonHelpers {

	val viewPath = "/WEB-INF/views/"
	
	val recipesPath = "recipes.json"
	
	var recipes = readRecipes
	
	private def readRecipes: Map[String, Recipe] = {
		try {
			val file = scala.io.Source.fromFile(new java.io.File(recipesPath))
			val string = file.getLines.mkString
			val recipes = parse(string).extract[Map[String, Recipe]]
			file.close
			recipes
		} catch {
			case e: java.io.FileNotFoundException => Map[String, Recipe]()
		}
	}
	
	private def storeRecipes {
		val writer = new java.io.PrintWriter(new java.io.File(recipesPath))
		val string: String = Json(recipes).toString
		writer.write(string)
		writer.close()
	}	
    
  before("/*") {
	contentType = "text/html"
  }
  
  before("/api/json/*") {
    contentType = "application/json;charset=UTF-8"
  }
  
  // ----------------------------------------------------------
  // Dev - für HTML
  
  get("/") {
    redirect("/recipes?methode=read")
  }
  
  get("/recipes") {
	params("methode") match {
		case "read" => layoutTemplate(viewPath + "recipe-list.ssp", "recipes" -> recipes)
		case "create" => layoutTemplate(viewPath + "recipe-create.ssp")
	}
  }
    
  get("/recipes/:name") {
	params("methode") match {
		case "read"	=> getRecipe(params("name"), 
				recipe 	=> layoutTemplate(viewPath + "recipe-view.ssp", "recipe" -> recipe), 
				name 	=> layoutTemplate("recipe-not-found.ssp", "name" -> name))
		case "update" => getRecipe(params("name"), 
				recipe 	=> layoutTemplate(viewPath + "recipe-create.ssp", "recipe" -> recipe), 
				name 	=> layoutTemplate("recipe-not-found.ssp", "name" -> name))
		case "delete" => {
			val name = params("name")
			recipes =  recipes - name
			storeRecipes
			redirect("/")
		}
	}
  }
    
  post("/recipes") {
	val name = params("name")
	val ingredients = params("ingredients").split("\n").toList
	val desc = params("desc")
	recipes = recipes + ( name -> new Recipe(name, ingredients, desc) )
	storeRecipes
	redirect("/")
  }
  
  // GET - read
  
   get("/api/json/recipes") {
	Json(recipes)
  }  
  
  get("/api/json/recipes/:name") {
	getRecipe(params("name"), 
		recipe	=> Json(recipe),
		name 	=> NotFound("Kein Rezept mit dem Namen: "+name))
  }  
  
  // POST - create
  
	post("/api/json/recipes") {
		val recipe = parse(request.body).extract[Recipe]
		recipes = recipes + ( recipe.name -> recipe )
		storeRecipes
	}
  
  // PUT - update
  
	put("/api/json/recipes") {
		val newRecipes = parse(request.body).extract[Map[String, Recipe]]
		recipes = recipes ++ newRecipes
		storeRecipes
	}
  
	put("/api/json/recipes/:name") {
		getRecipe(params("name"), 
			recipe	=> {
				val recipe = parse(request.body).extract[Recipe]
				recipes = recipes + ( params("name") -> recipe )
				storeRecipes }, 
			name 	=> NotFound("Kein Rezept mit dem Namen: "+name))
	}
  
  // DELETE - delete
  
  delete("/api/json/recipes") {
	recipes = recipes.empty
	storeRecipes
  }
  
  delete("/api/json/recipes/:name") {
	val name = params("name")
	getRecipe(name, 
			recipe	=> recipes = recipes - name, 
			name 	=> NotFound("Kein Rezept mit dem Namen: "+name))	
  }
 
  
  private def getRecipe(name: String, found: (Recipe) => Any, notFound: (String) => Any ) = {
	recipes.toMap.get(name) match {
		case Some(recipe) => {
			found(recipe)	
		}
		case None => {
			notFound(name)
		}	
	}     
  }
  
  notFound {
    // remove content type in case it was set through an action
    contentType = null
    // Try to render a ScalateTemplate if no route matched
    findTemplate(requestPath) map { path =>
      contentType = "text/html"
      layoutTemplate(path)
    } orElse serveStaticResource() getOrElse resourceNotFound()
  }
}
