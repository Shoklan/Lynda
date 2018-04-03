#
##

#

- Example: `PUT sales order 123`
- You can query information about the cluster using `GET _cat/` along with what you're looking at.
- You can then append it with:
  * `?v` is for formatting the return.
  * `nodes?v` to see node Information.
  * `indices?v` to see any indices in the cluster.
  *
- You can create an indice using `PUT /<indice>`.
- You can create data in an index using `PUT /<indice>/<type>/<id>`
- You can also put information in using a JSON object.
- You can delete an item using `DELETE <indice>`.
- You don't want to push up data one by one.
- You use the **Bulk API** instead.
- This is at `/_bulk`.
- Make sure that you're using the `curl --data-binary` flag.
- 

#

#

#

#

#

#
