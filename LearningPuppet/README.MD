# Learning Puppet
## Josh Samuelson

# Welcome
- Puppet keeps your servers in the expected state using defined code.
- What you should know:
  * Linux System Administration Basics.
  * Command Line Shell and Editor Experience.
  * VM should have: 1GB Free; 2 CPU Cores.
- *Puppet* is a language for describing the desired state of your infrustructure.
- It is usually described as a **COnfiguration Manamgement Tool**.
- It focused on modeling your infrastructure.
- It is also reasonably safe Cross Platform.
- Concepts such as Files and Services are abstracted into what are called **Resources**.
  * `..` are Single Units of configuration in Puppet.
- Those *resources* can be grouped into custom groups or classes.
- **Profiles** are just classes that group together classes and configurations.
- Each machine will get assigned a single **Role Class**.
- It can automate reports for inventory and compliance.
- It runs every 30 minutes to check for any discrepancies.
- A **Node** is an individual server of device managed by Puppet.
- A **Class** is a collection of Puppet code that makes sense as a logical group.
- A **Manifest** is a text file for holding Puppet code.
- A **Profile** is a class that defines a specific set of configurations.
- A **Role** is a clas that defines the business role of a node.
- You download teh

# Setting Up a Dev Environment
- We'll be using virtualbox.
- We'll also be using Open Source Puppet.
- There are a whole bunch of extra features for the Enterprise version.
- The **Vagrantfile** stores the configuration for our server.
- To initialize it, you run `vagrant up`.
- You can login using `vagrant ssh`.
- You can install the **Puppet Master** using:
  * `rpm -Uvh https://yum.puppetlabs.com/puppet5/puppet5-release-el-7.noarch.rmp` for Fedora/Red Hat.
  * `dpkg -i https://apt.puppetlabs.com/puppet-release-xenial.deb` for fedora.
-

# First Steps With Puppet

# Managing More Nodes

# Modules

# Conclusion

# Research:
- Vagrant?

# Reference:
- [Puppet Forge](www.forge.puppet.com)
