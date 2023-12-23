package org.amumu.rule.tree.infra.functions.ruletree.path;

import com.fasterxml.jackson.annotation.JsonIgnore;
import lombok.Data;
import java.util.List;

/**
 * 规则执行路径
 */
@Data
public class Path {

    private String id;
    private String op;
    private String type;
    private String name;
    private String param;
    private Boolean result;
    private List<String> val;
    @JsonIgnore
    private Path next;
    private List<Path> brothers;
}
