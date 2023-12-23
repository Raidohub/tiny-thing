package org.amumu.rule.tree.infra.functions.ruletree;

import org.amumu.rule.tree.domain.RuleTreeConditionDomain;
import org.amumu.rule.tree.domain.model.RuleTreeParam;
import org.amumu.rule.tree.infra.functions.ruletree.node.Node;
import org.amumu.rule.tree.infra.functions.ruletree.node.NodeWrapper;
import org.amumu.rule.tree.infra.utils.JsonUtil;
import org.springframework.stereotype.Component;
import java.util.ArrayList;
import java.util.List;

@Component
public class NodeChainFactory {

    public final ThreadLocal<Node> root = new ThreadLocal<>();
    public final ThreadLocal<Node> curr = new ThreadLocal<>();

    public void setupChain(String param, RuleTreeConditionDomain condition, Boolean result) {
        Node currNode = curr.get();
        Node newNode = this.buildNode(condition, param, result);
        if (newNode.getPid().equals(currNode.getPid())) {
            setupBrother(newNode);
            curr.set(newNode);
            return;
        }
        currNode.setSon(newNode);
        curr.set(newNode);
    }

    public void setupBrother(Node newNode) {
        Node currNode = curr.get();
        List<Node> brothers = curr.get().getSons();
        if (brothers == null) {
            brothers = new ArrayList<>();
            currNode.setSons(brothers);
        }
        brothers.add(newNode);
    }

    public NodeWrapper buildNode(Boolean result, RuleTreeParam param) {
        NodeWrapper nodeWrapper = new NodeWrapper();
        nodeWrapper.setParam(JsonUtil.obj2JsonStr(param));
        nodeWrapper.setRoot(root.get());
        nodeWrapper.setResult(result);
        this.clear();
        return nodeWrapper;
    }

    public void printPath() {
        System.out.println(JsonUtil.obj2JsonStr(this.root.get()));
        this.clear();
    }

    public void clear() {
        this.root.remove();
        this.curr.remove();
    }

    public void initPath(String id, String name) {
        Node root = new Node();
        root.setId(id);
        root.setName(name);
        this.root.set(root);
        this.curr.set(root);
    }

    private Node buildNode(RuleTreeConditionDomain condition, String param, Boolean result) {
        Node node = new Node();
        node.setResult(result);
        node.setParam(param);

        node.setName(condition.getName());
        node.setType(condition.getType());
        node.setVal(condition.getVal());
        node.setPid(condition.getPid());
        node.setId(condition.getId());
        node.setOp(condition.getOp());
        return node;
    }
}
