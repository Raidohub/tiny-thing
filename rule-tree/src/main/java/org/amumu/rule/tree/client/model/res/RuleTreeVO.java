package org.amumu.rule.tree.client.model.res;

import lombok.Data;

import java.util.Date;

@Data
public class RuleTreeVO {
    private Long id;
    private Long version;
    private String bizCode;
    private String bizName;
    private String condition;
    private Date gmtCreate;
    private Date gmtModified;
    private Long userId;
    private String userName;
    private String desc;
}
