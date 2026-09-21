import vitis
import shutil

# Create workspace
client = vitis.create_client()
client.set_workspace(path="./workspace")

# Derive system project from template
proj = client.create_sys_project(name="siege_xrt", platform="/opt/xilinx/platforms/xilinx_u55c_gen3x16_xdma_3_202210_1/xilinx_u55c_gen3x16_xdma_3_202210_1.xpfm", template="vitis_examples/host_xrt/host_memory_simple_xrt")

# Clean up template
client.delete_component(name="siege_xrt_krnl_vadd")
status = proj.remove_container(name="krnl_vadd")
status = proj.add_container(name="siege_container")
proj.add_precompiled_kernel("../../../siege_axi.xo", ['siege_container'])

# Copy host source
shutil.copyfile('./src/host.cpp', './workspace/siege_xrt_host/src/host.cpp')

# Build host app
comp = client.get_component(name="siege_xrt_host")
comp.build()

# Copy build script
shutil.copyfile('./src/siege_container-link.cfg', './workspace/siege_xrt/hw_link/siege_container-link.cfg')

# Build hw
proj.build(target="hw")

vitis.dispose()
