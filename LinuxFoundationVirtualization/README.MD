# Linux Foundation Cert Prep: Virtualization
## Scott Simpson

# Virtual Machine Basics
- We're going to discuss how to set these up, manage them and migrate them.
- Some VMs allow for nested virtualizations.
- Make sure that your CPU supports Hypervisor Virtualization and that it is enabled in the BIOS.
- You can check if it supports it via linux using: `egrep "(vmx|svm)" /proc/cpuinfo`
- We can **Running Bare Metal** when there is nothing between the OS and the hardware.
- We cannot have multiple OS's using the Bare Metal Hardware and so we need the **Hypervisor** to act as a load balancer for this stuff.
- We can the OS running on the Hypervisor a **Guest OS** and the OS the Hypervisor is running on as the **Host**.
- There are two kinds of virtualization: **Full Virtualization** and **Paravirtualization**.
- For the *Full Virt*, it creates emulated devices and then passes them along to real hardware.
- A common package in Linux to do this is *Quick Emulator (QEMU)* and *Kernel Virtual Machine (KVM)*.
- For the *Para Virt*, the Guest OS must be informed that it is virutalized.
- This allows the guest to communicate with the Hardware.
- QEMU+KVM does some parts both ways.
- If you want it to be full, then you'll need to use Xen.
- We can use either `qemu` or other libraries to manage the VMs.
- To install what we'll be using we'll use `sudo apt install qemu qemu-kvm`.
- If you want the headless version, then you'll want `qeumu-headless`.
- Qemu can emulate lots of architectures but not ARM.
- We will need some storage for our VMs.
- VMs can use real disks as well but we're not going to do that today.
- We can create or convert images with `qemu-img`.
- A *Raw Image* is a file that represents a disk; all the space is taken up - think provisioned.
- **QCOW2** stands for *QEMU copy on Write*.
- This creates a file that represents a disk but is not thick provisioned.
```
qemu-img create -f qcow2 my-image.qcow2 20G
```
- Once you have the disk created, you can use a .ISO file that you download to setup and install an OS
```
qemu-system-x86_64 -cdrom <image-file>.iso <disk-image>
```
- By default, it will start a VM with 64MBs of RAM.
- You can append the flag `-m 2G` to tell it to use 2GBs of RAM.
- You can tell it to use KVM with the flag `-enable-kvm`.
- You can run an image with `qemu-system-x86_64 my-image.qcow2`.
- Now that we have a VM to work with, we can add hardware to the VM host.
- You can refuse to provide it with a network adapter by passing `-net none`.
- You can tell it to use the paravirtualized video driver by passing `-vga qxl`.
- Using virsh, it will describe VMs in what are called Domain XML.
- You will want to install virsh using `sudo apt install virt-manager`.
- You can install using virsh with `virt-install`
- There are lots of options:
  * `--name` the name of the domain.
  * `--memory` the amount of RAM.
  * `--disk size=60,format=qcow2,path=<path/to/vm/image>`
  * `--cdrom <path-to-ISO>`.
- We're going to look over managing domain states in virsh:
  * `start <domain>` to boot a domain.
  * `stop <domain>` to ungracefully shutdown a domain.
  * `suspend <domain>` to pause or freeze a domain.
  * `resume <domain>` to unpause or unfreeze a domain.
  * `shutdown <domain>` to forcefully power down a domain.
- You can list all the domains in all states in virsh using `virsh list --all`.
- Simply because a domain is running does not mean it will hae a GUI.
- It will use a protocol called SPICE by default but you can configure others - such as VNC.
- You can view it using `virt-viewer <domain>`.
- You can see the domain display using the command `virsh domdisplay <domain>`.
- You can see the domain info using the command `virsh dominfo <domain>`.
- You can set the amount of memory using `virsh setmem <domain> <n>M`.
- You can attach a disk to a VM using `virsh attach-disk <domain> --source <path-to-disk>`.
- You can detach a disk to a VM using `virsh detach-disk <domain> --source <path-to-disk>`.
- Snapshots simply make some changes and records those changes.
- Clones create whole new copies of the images.
- You can install LXC using `sudo apt install lxc1`.


# Research:
- LXC?


# Reference:
