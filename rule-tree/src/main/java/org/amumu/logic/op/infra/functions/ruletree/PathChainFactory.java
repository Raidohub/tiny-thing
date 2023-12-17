package org.amumu.logic.op.infra.functions.ruletree;

import org.amumu.logic.op.domain.RuleTreeConditionDomain;
import org.amumu.logic.op.domain.model.RuleTreeParam;
import org.amumu.logic.op.infra.functions.ruletree.path.Path;
import org.amumu.logic.op.infra.functions.ruletree.path.PathWrapper;
import org.amumu.logic.op.infra.utils.JsonUtil;
import org.springframework.stereotype.Component;

import java.util.Optional;

@Component
public class PathChainFactory {

    private final ThreadLocal<Path> root = new ThreadLocal<>();
    private final ThreadLocal<Path> curr = new ThreadLocal<>();

    public void initPath() {
        Path root = new Path();
        root.setName("root");
        root.setId("-1");
        this.root.set(root);
        this.curr.set(root);
    }

    private Path buildPath(String id, String name, Boolean result) {
        Path path = new Path();
        path.setResult(result);
        path.setName(name);
        path.setId(id);
        return path;
    }

    private void chainNext(Path next) {
        curr.get().setNext(next);
    }

    public void chainPath(RuleTreeConditionDomain condition, boolean result) {
        Path next = Optional.ofNullable(condition)
                .map(e -> this.buildPath(e.getId(), e.getName(), result))
                .orElse(null);
        this.chainNext(next);
        curr.set(curr.get().getNext());
    }

    public PathWrapper buildRuleTreePath(Boolean result, RuleTreeParam ruleTreeParam) {
        PathWrapper pathWrapper = new PathWrapper();
        pathWrapper.setRuleTreeParam(JsonUtil.obj2JsonStr(ruleTreeParam));
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
