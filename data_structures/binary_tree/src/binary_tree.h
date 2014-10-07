typedef struct BinaryTree_Node {
	void *data;
	struct BinaryTree_Node *left,
		*right;
} BinaryTree_Node_t;

typedef struct {
	int size;
	void (*freeData)(void *data);
	BinaryTree_Node_t *root;
} BinaryTree_Tree_t;

BinaryTree_Tree_t *BinaryTree_create(void ( *freeData)(void *data));
void BinaryTree_free(BinaryTree_Tree_t *tree);
void BinaryTree_insertLeft(
	BinaryTree_Tree_t *tree, BinaryTree_Node_t *node,
	void *data
);
void BinaryTree_insertRight(
	BinaryTree_Tree_t *tree, BinaryTree_Node_t *node,
	void *data
);
void *BinaryTree_removeLeft(
	BinaryTree_Tree_t *tree, BinaryTree_Tree_t *node
);
void *BinaryTree_removeRight(BinaryTree_Tree_t *tree, BinaryTree_Node_t *node);
void *BinaryTree_merge(
	BinaryTree_Tree_t *leftTree, BinaryTree_Tree_t *rightTree
);
void *BinaryTree_isBranch(const BinaryTree_Node_t *node);
void *BinaryTree_isLeaf(const BinaryTree_Node_t *node);
