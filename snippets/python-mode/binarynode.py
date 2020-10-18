# -*- mode: snippet -*-
#name : binarynode
#key : binarynode
#contributor : Rieljun
# --
from binarytree import Node as BaseNode


class Node(BaseNode):
    @property
    def val(self):
        return self.value

    @property
    def data(self):
        return self.value$0
