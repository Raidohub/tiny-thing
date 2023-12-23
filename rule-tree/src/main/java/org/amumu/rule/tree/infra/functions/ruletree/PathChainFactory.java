package org.amumu.rule.tree.infra.functions.ruletree;

import org.amumu.rule.tree.domain.RuleTreeConditionDomain;
import org.amumu.rule.tree.domain.model.RuleTreeParam;
import org.amumu.rule.tree.infra.functions.ruletree.path.Path;
import org.amumu.rule.tree.infra.functions.ruletree.path.PathWrapper;
import org.amumu.rule.tree.infra.utils.JsonUtil;
import org.springframework.stereotype.Component;
import java.util.ArrayList;
import java.util.List;

@Component
public class PathChainFactory {

    public final ThreadLocal<Path> root = new ThreadLocal<>();
    public final ThreadLocal<Path> curr = new ThreadLocal<>();

    public void initPath(String id, String name) {
        Path root = new Path();
        root.setId(id);
        root.setName(name);
        this.root.set(root);
        this.curr.set(root);
    }

    private Path buildNode(RuleTreeConditionDomain condition, String param, Boolean result) {
        Path path = new Path();
        path.setResult(result);
        path.setParam(param);

        path.setName(condition.getName());
        path.setType(condition.getType());
        path.setVal(condition.getVal());
        path.setId(condition.getId());
        path.setOp(condition.getOp());
        return path;
    }

    public void next(String param, RuleTreeConditionDomain condition, Boolean result) {
        Path next = this.buildNode(condition, param, result);
        curr.get().setNext(next);
        curr.set(next);
    }

    public void brother(String param, RuleTreeConditionDomain condition, Boolean result) {
        List<Path> brothers = curr.get().getBrothers();
        if (brothers == null) {
            brothers = new ArrayList<>();
            curr.get().setBrothers(brothers);
        }
        Path brother = this.buildNode(condition, param, result);
        brothers.add(brother);
        curr.set(brother);

    }

    public PathWrapper buildRuleTreePath(Boolean result, RuleTreeParam ruleTreeParam) {
        PathWrapper pathWrapper = new PathWrapper();
        pathWrapper.setParam(JsonUtil.obj2JsonStr(ruleTreeParam));
        pathWrapper.setRoot(root.get());
        pathWrapper.setResult(result);
        this.clear();
        return pathWrapper;
    }

    public void printPath() {
        Path root = this.root.get();
        while (root != null) {
            System.out.println(JsonUtil.obj2JsonStr(root));
            root = root.getNext();
        }
        this.clear();
    }

    public void clear() {
        this.root.remove();
        this.curr.remove();
    }

}
