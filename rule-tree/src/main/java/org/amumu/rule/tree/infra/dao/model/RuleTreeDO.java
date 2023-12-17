package org.amumu.rule.tree.infra.dao.model;

import com.mybatisflex.annotation.Column;
import com.mybatisflex.annotation.Id;
import com.mybatisflex.annotation.KeyType;
import com.mybatisflex.annotation.Table;
import lombok.Data;

import java.util.Date;

@Data
@Table("rule_tree")
public class RuleTreeDO {
    @Id(keyType = KeyType.Auto)
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

    @Column(isLogicDelete = true)
    private Boolean deleted;
}
