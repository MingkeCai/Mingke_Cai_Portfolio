from flask import Flask, request, jsonify
import psycopg2
import json
from flask_cors import CORS

app = Flask(__name__)

# Enable Cross-Origin Resource Sharing (CORS)
CORS(app)

# Database connection parameters
DB_HOST = "db-test.czzyqz8xmvxi.us-east-1.rds.amazonaws.com"
DB_NAME = "postgres"
DB_USERNAME = "postgres"
DB_PASSWORD = "postgres"

# SQL query to search for jobs matching a list of skills
def find_jobs_by_skills(skills):
    query = f"""
        SELECT job_title, job_level, job_summary
        FROM cleaned_data
        WHERE {' AND '.join([f"job_skills ILIKE %s"] * len(skills))}
        LIMIT 5
    """
    print(query)

    # Format skills for SQL LIKE query
    formatted_skills = [f"%{skill}%" for skill in skills]

    try:
        print("before connection")
        # Establish a database connection
        conn = psycopg2.connect(host=DB_HOST, user=DB_USERNAME, password=DB_PASSWORD, dbname=DB_NAME)
        print("after connection")
        cur = conn.cursor()
         # Execute the query with the skills formatted
        cur.execute(query, formatted_skills)
        # Fetch results from the query
        results = cur.fetchall()
         # Structure the data as a list of dictionaries
        jobs = [{"job_title": job[0], "job_level": job[1], "job_summary": job[2]} for job in results]
        return json.dumps(jobs, indent=2)
    except Exception as e:
        print("An error occurred:", e)
    finally:
        # Ensure all resources are released properly
        if 'cur' in locals():
            cur.close()
        if 'conn' in locals():
            conn.close()

# SQL query to find top locations for specific job title and level
def get_recommended_locations(job_title, job_level):
    query = """
        SELECT job_location, COUNT(*)
        FROM cleaned_data
        WHERE job_title ILIKE %s AND job_level ILIKE %s
        GROUP BY job_location;
    """
    try:
        conn = psycopg2.connect(host=DB_HOST, user=DB_USERNAME, password=DB_PASSWORD, dbname=DB_NAME)
        cur = conn.cursor()
        cur.execute(query, (f"%{job_title}%", f"%{job_level}%"))
        # Organize results as a list of dictionaries
        results = cur.fetchall()
        locations = [{"job_location": loc[0], "job_count": loc[1]} for loc in results]
        return json.dumps(locations, indent=2)
    finally:
        # Ensure all resources are released properly
        if 'cur' in locals():
            cur.close()
        if 'conn' in locals():
            conn.close()

@app.route('/find_jobs', methods=['POST'])
def handle_find_jobs():
    print("in find jobs...")
     # Retrieve skills from the POST request
    skills = request.form['skills'].split(',')
    print("Received skills:", skills)
    # Get job recommendations based on skills
    result = find_jobs_by_skills(skills)
    print("result!")
    print(result)
    # Return results as JSON
    return jsonify(result)

@app.route('/find_locations', methods=['POST'])
def handle_find_locations():
     # Retrieve job title and level from POST request
    job_title = request.form['job_title']
    job_level = request.form['job_level']
    # Get location recommendations based on job title and level
    result = get_recommended_locations(job_title, job_level)
    return jsonify(result)


if __name__ == '__main__':
    app.run(debug=True, host='0.0.0.0', port=5001)
