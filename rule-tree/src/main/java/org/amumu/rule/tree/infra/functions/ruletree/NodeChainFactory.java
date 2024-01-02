package org.amumu.rule.tree.infra.functions.ruletree;

import org.amumu.rule.tree.domain.RuleTreeConditionDomain;
import org.amumu.rule.tree.domain.model.RuleTreeParam;
import org.amumu.rule.tree.infra.functions.ruletree.node.Node;
import org.amumu.rule.tree.infra.functions.ruletree.node.NodeWrapper;
import org.amumu.rule.tree.infra.utils.JsonUtil;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;

@Component
public class NodeChainFactory {

    private final ThreadLocal<Node> root = new ThreadLocal<>();
    private final ThreadLocal<Node> curr = new ThreadLocal<>();
    private final ThreadLocal<LinkedList<Node>> backtrace = new ThreadLocal<>();

    /**
     * 设置链表头节点
     * @param param
     * @param condition
     * @param result
     */
    public void inChain(String param, RuleTreeConditionDomain condition, Boolean result) {
        Node currNode = this.curr.get();
        Node newNode = this.buildNode(condition, param, result);
        if (CollectionUtils.isEmpty(currNode.getSons())) {
            this.initSon(newNode);
            this.curr.set(newNode);

            return;
        }
        List<Node> sons = currNode.getSons();
        sons.add(newNode);
        this.curr.set(newNode);
    }

    private void initSon(Node newNode) {
        List<Node> sons = new ArrayList<>();
        sons.add(newNode);
        Node currNode = this.curr.get();
        currNode.setSons(sons);
    }

    /**
     * 回溯栈
     */
    public void setupBacktracePoint() {
        LinkedList<Node> backtrace = this.backtrace.get();
        if (CollectionUtils.isEmpty(backtrace)) {
            backtrace = new LinkedList<>();
            this.backtrace.set(backtrace);
        }
        backtrace.add(this.curr.get());
    }

    public void resetCurr() {
        LinkedList<Node> backTraceList = this.backtrace.get();
        Node backTrace = backTraceList.peekLast();
        this.curr.set(backTrace);
    }

    public void backtracePop() {
        this.backtrace.get().pollLast();
    }

    public NodeWrapper buildNode(Boolean result, RuleTreeParam param) {
        NodeWrapper nodeWrapper = new NodeWrapper();
        nodeWrapper.setParam(JsonUtil.obj2JsonStr(param));
        nodeWrapper.setRoot(this.root.get());
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

    public Node buildNode(RuleTreeConditionDomain condition, String param, Boolean result) {
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
