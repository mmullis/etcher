<!DOCTYPE html PUBLIC '-//W3C//DTD XHTML 1.0 Strict//EN'
          'http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd'>
<html lang="en" xml:lang="en" xmlns="http://www.w3.org/1999/xhtml">
  <head>  
    <meta content="text/html; charset=UTF-8" http-equiv="Content-Type"></meta>
    <title>Etcher Example 1</title>
    <style type="text/css">
        .row1 {
	    background-color: #bee;
	}
	.row2 {
	    background-color: #eeb;
	}
    </style>
  </head>
  <body>
    <h1>List of film director and actor collaborations</h1>
    <p><strong>NOTE: This list was taken from 
       <a href="http://en.wikipedia.org/wiki/List_of_film_director_and_actor_collaborations">
          Wikipedia</a></strong>
    </p>
    <ul>
    {% for collab in collaborations %}
      <li>
        <div class="director">
          <span class="label">Director:</span>
          <span class="person_name"> 
            {% if collab.director.last_name %}{{ collab.director.last_name }}, {% endif %}
            {{ collab.director.first_name }}
          </span>
        </div>
        <div class="actor">
          <span class="label">Actor:</span>
          <span class="person_name"> 
            {% if collab.actor.last_name %}{{ collab.actor.last_name }}, {% endif %}
            {{ collab.actor.first_name }}
          </span>
        </div>
        <ol>
        {% for film in collab.films %}
          <li class="{% cycle 'row1' 'row2' %}">
            <span class="film_year">{{ film.year }}</span>:
            <span class="film_title">{{ film.title }}</span>
          </li>
        {% endfor %}
        </ol>
      </li>
    {% endfor %}
    </ul>
  </body>
</html>

