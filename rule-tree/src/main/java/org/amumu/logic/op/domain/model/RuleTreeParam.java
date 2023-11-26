package org.amumu.logic.op.domain.model;

import java.util.Map;

public class RuleTreeParam {

    private String id;

    private Map<String, String> extra;

    public String getId() {
        return id;
    }

    public void setId(String id) {
        this.id = id;
    }

    public Map<String, String> getExtra() {
        return extra;
    }

    public void setExtra(Map<String, String> extra) {
        this.extra = extra;
    }
}
