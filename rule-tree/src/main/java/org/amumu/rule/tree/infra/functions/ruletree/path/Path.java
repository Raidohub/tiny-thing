package org.amumu.rule.tree.infra.functions.ruletree.path;

import com.fasterxml.jackson.annotation.JsonIgnore;
import lombok.Data;

/**
 * 规则执行路径
 */
@Data
public class Path {

    private String id;
    private String name;
    private String param;
    private Boolean result;
    @JsonIgnore
    private Path next;
}
