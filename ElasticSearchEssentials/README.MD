# Elasticserach Essentials Training
## Ben Sullins

# Introduction and Overview
- Should know:
  * Javascript
  * Web Front End
  * Database Basics
- 'Where Did This Come From':
  1. Lucene ( 1999 ).
  2. Compass ( 2004 )
  3. Elasticsearch (2010 )
- The company **Elastic** provides the **ELK Stack**; which is Elastic Search, Logstash, Kibana.
- There are two technologies for getting data into an Elastic Search Cluster:
  * Logstash.
  * Beats.
- *Elastic Search* sits on top of those technologies.
- *Kibana* is used to for Analysis and Visualizations.
- **X-Pack** is a paid for extension.
- There are **4** Key advantages for this stack:
  1. Scalability.
  2. Near Real-Time.
  3. Schemaless
  4. Advanced Query Language.
- Use Cases:
  * Security/Log Analytics.
  * Marketing.
  * Operations.
  * Search.
- **Cluster**: Collection of our Nodes.
- **Node**: is what stores the data.
- **Index**: where the data is stored in the Node.
- **Types**: Types of indexes.
- **Document**: The actual data, in JSON, that refers to a single instance of data.
- **Shard**: a segment of the data.
- **Replica**: a copy of the shard.
- The default is to have 5 shards, and 1 replica.


# Setup
- The syntax for queries is `<REST VERB> <INDEX> <TYPE> <ID>`.
- Example: `PUT sales order 123`
- You can query information about the cluster using `GET _cat/` along with what you're looking at.
- You can then append it with:
  * `?v` is for formatting the return.
  * `nodes?v` to see node Information.
  * `indices?v` to see any indices in the cluster.
- You can create an indice using `PUT /<indice>`.
- You can create data in an index using `PUT /<indice>/<type>/<id>`
- You can also put information in using a JSON object.
- You can delete an item using `DELETE <indice>`.


# Loading Data
- You don't want to push up data one by one.
- You use the **Bulk API** instead.
- This is at `/_bulk`.
- Make sure that you're using the `curl --data-binary` flag.
- There are 4 datatypes:
  * Core
    1. Text
    2. Numeric
    3. Boolean
    4. Binary
    5. Range.
  * Complex
    1. Array
    2. object
    3. Nested array of JSON objects.
  * Geo
    1. Point.
    2. Shape.
  * Specialized
    1. IP Addresses.
    2. Autocomplete Suggestions.
    3. Tokens.


# Querying Data
- To query all data in an indice, use `GET <indice>/<type>/_search`.
- This is also called a **Match All** query.
- You can use `match` inside a query json block to query Elastic Search for results.
- You can also use `boost` to Prioritize values you're looking for in a search.
- **Term Queries** cannot be used to refer to *text fields*.
- You can use a **Terms Query** along with a list of values to search for more than one item.
- You can use the **Range Keyword** to allow for keywords such as `gte` - *Greater Than Equal* - or `lte` - *Less Than Equal*.
- The `_analyze` endpoint can be used to investigate our indices.
- It will output what Elastic Search will do with test data submitted to it.
- There are a few different built in tokenizers:
  * *standard*: just breaks into tokens based on spaces and special characters get dropped.
  * *uax_url_email*: detects email addresses and url formats in text.
- Setting up your indexes with formatting ahead of time can do a lot of work for you.


# Analyzing Your Data
- **Aggregation** is done with the `aggs` keyword.
- Sending `"size": 0` will prevent actual search results from being returned.
- You can pass an **Aggregation Argument** using `"aggs"`.
- *Aggregations* work inside of the context of a query.
- **bool** means that multiple conditions can be matched.
- **filter** allows you to limit what shows up in the results.
- **global** tells Elastic Search to check all fields in the index.

# Presenting Your Insights

# Research:

# Reference:
