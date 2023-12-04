-- rule_tree --
USE tiny_thing;
CREATE TABLE rule_tree (
    `id` INT(11) NOT NULL AUTO_INCREMENT,
    `version` BIGINT NOT NULL COMMENT '版本',
    `biz_code` VARCHAR(255) NOT NULL COMMENT '业务编码',
    `biz_name` VARCHAR(255) NOT NULL COMMENT '业务名称',
    `condition` LONGTEXT NOT NULL COMMENT '规则',
    `gmt_create` TIMESTAMP DEFAULT NOW() COMMENT '创建时间【格林尼治】',
    `gmt_modified` TIMESTAMP DEFAULT NOW() COMMENT '修改时间【格林尼治】',
    `user_id` BIGINT NOT NULL COMMENT '修改用户ID',
    `user_name` VARCHAR(255) NOT NULL COMMENT '修改用户名称',
    `desc` VARCHAR(255) DEFAULT NULL COMMENT '描述',
    `deleted` TINYINT(1) DEFAULT 0 COMMENT '逻辑删除',
    PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_general_ci COMMENT = '规则树配置';

INSERT INTO `rule_tree` (`id`, `version`, `biz_code`, `biz_name`, `condition`,`gmt_create`, `gmt_modified`, `user_id`, `user_name`, `desc`, `deleted`) VALUES (1, 1, 'test', '测试', '{"id":"-1","name":"签证时效切流配置","op":null,"val":null,"type":"condition","conditions":{"id":"0","name":null,"op":null,"val":null,"type":"and","field":null,"conditions":[{"id":"1","name":"whitelist","op":null,"val":["true"],"type":"enabled","field":null,"conditions":null},{"id":"0","name":null,"op":null,"val":null,"type":"or","field":null,"conditions":[{"id":"1","name":"itemId","op":"in","val":["2","3"],"type":"int","field":"itemId","conditions":null},{"id":"-1","name":null,"op":null,"val":null,"type":"condition","field":null,"conditions":{"id":0,"name":null,"op":null,"val":null,"type":"or","field":null,"conditions":[{"name":"threshold","id":"2","op":"gte","val":["95"],"type":"int","field":"threshold","conditions":null},{"name":"isSeller","id":"3","op":"eq","val":["true"],"type":"boolean","field":"isSeller","conditions":null}]}}]}]}}', now(), now(), 1, 'admin', '测试', 0);