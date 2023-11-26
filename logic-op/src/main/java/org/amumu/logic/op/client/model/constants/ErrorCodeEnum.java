package org.amumu.logic.op.client.model.constants;

import lombok.Getter;

import javax.servlet.http.HttpServletResponse;
import java.text.MessageFormat;

/**
 * 错误码定义
 *
 * @author jockeys
 * @since 2020/4/6
 */
@Getter
public enum ErrorCodeEnum {
    /**
     * 400
     */
    BAD_REQUEST(HttpServletResponse.SC_BAD_REQUEST, "请求参数有误"),
    /**
     * JSON格式错误
     */
    JSON_FORMAT_ERROR(HttpServletResponse.SC_BAD_REQUEST, "JSON格式错误"),
    /**
     * 401
     */
    UNAUTHORIZED(HttpServletResponse.SC_UNAUTHORIZED, "请先进行认证"),
    /**
     * 403
     */
    FORBIDDEN(HttpServletResponse.SC_FORBIDDEN, "暂无权操作，如需授权，请联系管理员"),
    /**
     * 404
     */
    NOT_FOUND(HttpServletResponse.SC_NOT_FOUND, "未找到"),
    /**
     * 405
     */
    METHOD_NOT_ALLOWED(HttpServletResponse.SC_METHOD_NOT_ALLOWED, "请求方式不支持"),
    /**
     * 406
     */
    NOT_ACCEPTABLE(HttpServletResponse.SC_NOT_ACCEPTABLE, "不可接受该请求"),
    /**
     * 411
     */
    LENGTH_REQUIRED(HttpServletResponse.SC_LENGTH_REQUIRED, "请求长度受限制"),
    /**
     * 415
     */
    UNSUPPORTED_MEDIA_TYPE(HttpServletResponse.SC_UNSUPPORTED_MEDIA_TYPE, "该请求不支持的媒体类型"),
    CONVERTER_ERROR(HttpServletResponse.SC_UNSUPPORTED_MEDIA_TYPE, "类型转换异常"),
    /**
     * 416
     */
    REQUESTED_RANGE_NOT_SATISFIABLE(HttpServletResponse.SC_REQUESTED_RANGE_NOT_SATISFIABLE, "该请求不在请求满足的范围"),
    /**
     * 500
     */
    INTERNAL_SERVER_ERROR(HttpServletResponse.SC_INTERNAL_SERVER_ERROR, "服务器出了点问题，请稍后再试"),
    /**
     * 503
     */
    SERVICE_UNAVAILABLE(HttpServletResponse.SC_SERVICE_UNAVAILABLE, "请求超时"),
    /**
     * 消息异常
     */
    MSG_EXCEPTION(HttpServletResponse.SC_INTERNAL_SERVER_ERROR, "默认消息异常"),
    /**
     * tag
     */
    TAG_INEFFECTIVE(HttpServletResponse.SC_BAD_REQUEST, "标签未生效"),
    TAG_NOT_EXIST(HttpServletResponse.SC_BAD_REQUEST, "标签不存在"),
    //----------------------------------------------------业务异常----------------------------------------------------
    /**
     * Dept
     */
    DEPT_EXISTING_LOWER_LEVEL_DEPT(HttpServletResponse.SC_BAD_REQUEST, "当前部门存在下属部门，不允许删除"),
    DEPT_EXISTING_USER(HttpServletResponse.SC_BAD_REQUEST, "当前部门还存在用户，不允许删除"),
    DEPT_NAME_EXIST(HttpServletResponse.SC_BAD_REQUEST, "部门名称已经存在"),
    DEPT_PARENT_DEPT_CANNOT_MYSELF(HttpServletResponse.SC_BAD_REQUEST, "上级部门不能是当前部门"),
    /**
     * User
     */
    USER_DISABLED(HttpServletResponse.SC_BAD_REQUEST, "用户未激活或已锁定"),
    USER_NOT_EXIST(HttpServletResponse.SC_BAD_REQUEST, "用户不存在"),
    USER_OLD_PASSWORD_ERROR(HttpServletResponse.SC_BAD_REQUEST, "修改密码失败，旧密码错误"),
    USER_AVATAR_NOT_EMPTY(HttpServletResponse.SC_BAD_REQUEST, "用户头像不能为空"),
    USER_AVATAR_UPLOAD_FAIL(HttpServletResponse.SC_INTERNAL_SERVER_ERROR, "用户头像上传失败"),
    USER_CANNOT_UPDATE_SUPER_ADMIN(HttpServletResponse.SC_BAD_REQUEST, "不可以修改超级管理员"),
    USER_EXIST(HttpServletResponse.SC_BAD_REQUEST, "会员已存在"),
    USER_PHONE_NO_EXIST(HttpServletResponse.SC_BAD_REQUEST, "手机号已存在"),
    USER_PHONE_NO_BOUND(HttpServletResponse.SC_BAD_REQUEST, "手机号已绑定"),
    USER_USERNAME_EXIST(HttpServletResponse.SC_BAD_REQUEST, "用户名已存在"),
    USER_PHONE_INVALID(HttpServletResponse.SC_BAD_REQUEST, "手机号格式错误"),
    USER_EMAIL_EXIST(HttpServletResponse.SC_BAD_REQUEST, "Email已存在"),
    USER_EMAIL_INVALID(HttpServletResponse.SC_BAD_REQUEST, "Email格式错误"),
    USER_NOT_ONLINE(HttpServletResponse.SC_BAD_REQUEST, "用户已下线"),
    USER_CANNOT_RETREAT_CURRENT_MEMBER(HttpServletResponse.SC_BAD_REQUEST, "当前登陆用户无法强退"),
    USER_ELSEWHERE_LOGIN(HttpServletResponse.SC_UNAUTHORIZED, "您已在别处登录，请您修改密码或重新登录"),
    USER_USERNAME_OR_PWD_WRONG(HttpServletResponse.SC_UNAUTHORIZED, "用户名或密码错误"),
    USER_CAPTCHA_WRONG(HttpServletResponse.SC_UNAUTHORIZED, "验证码错误"),
    USER_STATUS_LOCKED(HttpServletResponse.SC_UNAUTHORIZED, "账号已锁定"),
    USER_POINT_NOT_ENOUGH(HttpServletResponse.SC_BAD_REQUEST, "积分不足"),
    USER_POINT_REWARD_MAX_TRY_COUNT(HttpServletResponse.SC_BAD_REQUEST, "积分奖励达到最大重试次数"),
    /**
     * admin
     */
    ADMIN_DOES_NOT_EXIST(HttpServletResponse.SC_BAD_REQUEST, "管理员不存在"),
    ADMIN_FORBIDDEN(HttpServletResponse.SC_BAD_REQUEST, "管理员没有权限"),
    ADMIN_STATUS_LOCKED(HttpServletResponse.SC_BAD_REQUEST, "账户已锁定，请联系管理员"),
    ADMIN_STATUS_INACTIVE(HttpServletResponse.SC_BAD_REQUEST, "账户未激活，请联系管理员"),
    ADMIN_USERNAME_EXIST(HttpServletResponse.SC_BAD_REQUEST, "管理员已存在"),
    ADMIN_PHONE_NO_EXIST(HttpServletResponse.SC_BAD_REQUEST, "管理员电话号码已存在"),
    ADMIN_EMAIL_EXIST(HttpServletResponse.SC_BAD_REQUEST, "管理员邮箱已存在"),
    ADMIN_MEMBER_CONNECTED_EXIST(HttpServletResponse.SC_BAD_REQUEST, "会员已绑定"),
    ADMIN_LOGOUT_SUCCESS(HttpServletResponse.SC_OK, "操作成功"),
    /**
     * Role
     */
    ROLE_NAME_EXIST(HttpServletResponse.SC_BAD_REQUEST, "角色名称已存在"),
    ROLE_KEY_EXIST(HttpServletResponse.SC_BAD_REQUEST, "角色权限已存在"),
    ROLE_ACCESS_DENIED(HttpServletResponse.SC_FORBIDDEN, "没有权限"),
    ROLE_NOT_EXIST(HttpServletResponse.SC_FORBIDDEN, "角色不存在"),
    /**
     * permission
     */
    PERMISSION_URI_METHOD_EXIST(HttpServletResponse.SC_BAD_REQUEST, "权限已存在"),
    /**
     * menu
     */
    MENU_PARENT_TITLE_EXIST(HttpServletResponse.SC_BAD_REQUEST, "菜单标题已存在"),
    MENU_PARENT_NAME_EXIST(HttpServletResponse.SC_BAD_REQUEST, "菜单名称已存在"),
    /**
     * shopping
     */
    SHOPPING_NOT_EXIST(HttpServletResponse.SC_BAD_REQUEST, "购物车商品不存在"),
    SHOPPING_NOT_BELONG_MEMBER(HttpServletResponse.SC_BAD_REQUEST, "购物车商品不属于用户"),
    SHOPPING_EXIST(HttpServletResponse.SC_BAD_REQUEST, "商品已加入购物车"),
    /**
     * favorite
     */
    FAVORITE_NOT_EXIST(HttpServletResponse.SC_BAD_REQUEST, "收藏商品不存在"),
    FAVORITE_NOT_BELONG_MEMBER(HttpServletResponse.SC_BAD_REQUEST, "收藏商品不属于用户"),
    FAVORITE_EXIST(HttpServletResponse.SC_BAD_REQUEST, "商品已收藏"),
    /**
     * payment
     */
    PAYMENT_NOT_EXIST(HttpServletResponse.SC_BAD_REQUEST, "支付单不存在"),
    PAYMENT_TYPE_NOT_EXIST(HttpServletResponse.SC_BAD_REQUEST, "支付类型不存在"),
    PAYMENT_STATUS_INVALID(HttpServletResponse.SC_BAD_REQUEST, "支付单状态非法"),
    PAYMENT_SYSTEM_FAIL(HttpServletResponse.SC_INTERNAL_SERVER_ERROR, "支付失败，请稍后重试"),
    /**
     * dist
     */
    DIST_CONNECT_EXIST(HttpServletResponse.SC_BAD_REQUEST, "会员已绑定赚客"),
    DIST_CONNECT_NOT_EXIST(HttpServletResponse.SC_BAD_REQUEST, "会员未绑定赚客"),
    DIST_CONNECT_STATUS_INVALID(HttpServletResponse.SC_BAD_REQUEST, "绑定赚客状态非法"),
    /**
     * io
     */
    EXPORT_TYPE_NOT_NULL(HttpServletResponse.SC_BAD_REQUEST, "导出类型不能为空"),

    /**
     * area
     */
    AREA_NAME_EXIST(HttpServletResponse.SC_BAD_REQUEST, "地区名已存在"),
    AREA_NOT_EXIST(HttpServletResponse.SC_BAD_REQUEST, "地区不存在"),
    AREA_NOT_MATCH_ADDRESS(HttpServletResponse.SC_BAD_REQUEST, "地址与地区不匹配"),
    AREA_DELIVERY_MEMBER_NOT_MATCH(HttpServletResponse.SC_BAD_REQUEST, "地址和会员不匹配"),
    /**
     * identity card
     */
    IDENTITY_CARD_NOT_EXIST(HttpServletResponse.SC_BAD_REQUEST, "实名认证信息不存在"),
    IDENTITY_CARD_DEFAULTED_EXIST(HttpServletResponse.SC_BAD_REQUEST, "已存在默认实名认证信息"),
    IDENTITY_CARD_COUNT_LIMIT(HttpServletResponse.SC_BAD_REQUEST, "实名认证信息超过限制"),
    IDENTITY_CARD_AUTHENTICATION_FAIL(HttpServletResponse.SC_BAD_REQUEST, "实名认证失败"),
    /**
     * item
     */
    SPU_NOT_EXIST(HttpServletResponse.SC_BAD_REQUEST, "商品不存在"),
    SPU_NAME_EXIST(HttpServletResponse.SC_BAD_REQUEST, "商品名称已存在"),
    SPU_STATUS_DISABLED(HttpServletResponse.SC_BAD_REQUEST, "商品不在售卖"),
    SPU_STATUS_INVALID(HttpServletResponse.SC_BAD_REQUEST, "商品状态更新非法"),
    SKU_STATUS_DISABLED(HttpServletResponse.SC_BAD_REQUEST, "sku不在售卖"),
    SKU_NOT_EXIST(HttpServletResponse.SC_BAD_REQUEST, "sku不存在"),
    /**
     * category
     */
    CATE_NOT_EXIST(HttpServletResponse.SC_BAD_REQUEST, "分类不存在"),
    CATE_EXIST(HttpServletResponse.SC_BAD_REQUEST, "分类名已存在"),
    /**
     * attr
     */
    ATTR_GROUP_NAME_EXIST(HttpServletResponse.SC_BAD_REQUEST, "规格组名称已存在"),
    ATTR_NAME_EXIST(HttpServletResponse.SC_BAD_REQUEST, "规格名称已存在"),
    ATTR_CODE_EXIST(HttpServletResponse.SC_BAD_REQUEST, "规格编码已存在"),
    ATTR_OPT_NAME_EXIST(HttpServletResponse.SC_BAD_REQUEST, "规格项名称已存在"),
    ATTR_OPT_CODE_EXIST(HttpServletResponse.SC_BAD_REQUEST, "规格项编码已存在"),
    /**
     * cms
     */
    ARTICLE_NOT_EXIST(HttpServletResponse.SC_BAD_REQUEST, "文章不存在"),
    ARTICLE_TITLE_EXIST(HttpServletResponse.SC_BAD_REQUEST, "文章标题已存在"),
    /**
     * sms
     */
    SMS_TEMPLATE_NAME_EXIST(HttpServletResponse.SC_BAD_REQUEST, "短信模板名称已存在"),
    SMS_TEMPLATE_TYPE_DEFAULTED_UNICODE(HttpServletResponse.SC_BAD_REQUEST, "当前类型已存在默认模板"),
    SMS_TEMPLATE_TYPE_DEFAULTED_NOT_EXIST(HttpServletResponse.SC_BAD_REQUEST, "当前类型默认模板不存在"),
    /**
     * band
     */
    BRAND_NAME_EXIST(HttpServletResponse.SC_BAD_REQUEST, "品牌名称已存在"),
    BRAND_CODE_EXIST(HttpServletResponse.SC_BAD_REQUEST, "品牌编码已存在"),
    /**
     * warehouse
     */
    WAREHOUSE_DISABLED(HttpServletResponse.SC_BAD_REQUEST, "仓库不可用"),
    WAREHOUSE_STATUS_INVALID(HttpServletResponse.SC_BAD_REQUEST, "仓库状态非法"),
    WAREHOUSE_NOT_EXIST(HttpServletResponse.SC_BAD_REQUEST, "仓库不存在"),
    WAREHOUSE_CODE_EXIST(HttpServletResponse.SC_BAD_REQUEST, "仓库编码已存在"),
    WAREHOUSE_NAME_EXIST(HttpServletResponse.SC_BAD_REQUEST, "仓库名称已存在"),
    WAREHOUSE_SKU_EXIST(HttpServletResponse.SC_BAD_REQUEST, "仓库SKU已建立"),
    WAREHOUSE_SKU_STOCK_OUT(HttpServletResponse.SC_BAD_REQUEST, "仓库SKU库存不足"),
    WAREHOUSE_SKU_STOCK_UPDATE_ERROR(HttpServletResponse.SC_BAD_REQUEST, "仓库SKU库存更新失败"),
    WAREHOUSE_SKU_NOT_EXIST(HttpServletResponse.SC_BAD_REQUEST, "仓库SKU库存不存在"),
    WAREHOUSE_SKU_INIT_INVALID(HttpServletResponse.SC_BAD_REQUEST, "仓库SKU初始化不正确，已经存在该SKU"),
    WAREHOUSE_SKU_STOCK_REQ_QRY_CONDITION_MISS(HttpServletResponse.SC_BAD_REQUEST, "仓库SKU查询缺少仓库ID或skuId"),
    /**
     * promo
     */
    PROMO_NOT_EXIST(HttpServletResponse.SC_BAD_REQUEST, "优惠不存在"),
    PROMO_EXIST(HttpServletResponse.SC_BAD_REQUEST, "优惠已存在"),
    PROMO_STATUS_INVALID(HttpServletResponse.SC_BAD_REQUEST, "优惠状态非法"),
    PROMO_DETAIL_NOT_EXIST(HttpServletResponse.SC_BAD_REQUEST, "优惠详细不存在"),
    PROMO_ITEM_NOT_EXIST(HttpServletResponse.SC_BAD_REQUEST, "优惠商品不存在"),
    PROMO_CODE_EXIST(HttpServletResponse.SC_BAD_REQUEST, "优惠编码已存在"),
    PROMO_TYPE_DEFAULTED_UNICODE(HttpServletResponse.SC_BAD_REQUEST, "相同类型仅允许存在一个默认优惠"),
    PROMO_PRICE_GREATER_THAN_SALE_PRICE(HttpServletResponse.SC_BAD_REQUEST, "优惠价高于销售价"),
    PROMO_MANAGER_SCALE_METHOD_INVOKE_INVALID(HttpServletResponse.SC_BAD_REQUEST, "优惠 manager scale 方法非法调用"),
    /**
     * coupon
     */
    COUPON_NAME_EXIST(HttpServletResponse.SC_BAD_REQUEST, "优惠卷名已存在"),
    COUPON_NOT_EXIST(HttpServletResponse.SC_BAD_REQUEST, "优惠卷不存在"),
    COUPON_DETAIL_NOT_EXIST(HttpServletResponse.SC_BAD_REQUEST, "优惠卷详细不存在"),
    COUPON_RECEIVE_LIMIT(HttpServletResponse.SC_BAD_REQUEST, "已达优惠卷限制"),
    COUPON_STOCK_OUT(HttpServletResponse.SC_BAD_REQUEST, "优惠卷已领完"),
    COUPON_CODE_EXIST(HttpServletResponse.SC_BAD_REQUEST, "优惠卷编码已存在"),
    COUPON_ISSUE_NOT_READY(HttpServletResponse.SC_BAD_REQUEST, "未到优惠卷领取时间"),
    COUPON_STATUS_INVALID(HttpServletResponse.SC_BAD_REQUEST, "优惠卷状态非法"),
    COUPON_RECEIVE_LIMIT_DIST(HttpServletResponse.SC_BAD_REQUEST, "非赚客不能领取优惠卷"),
    /**
     * red envelop
     */
    RED_ENVELOP_ISSUE_NOT_READY(HttpServletResponse.SC_BAD_REQUEST, "未到红包领取时间"),
    RED_ENVELOP_NOT_EXIST(HttpServletResponse.SC_BAD_REQUEST, "红包不存在"),
    RED_ENVELOP_DETAIL_NOT_EXIST(HttpServletResponse.SC_BAD_REQUEST, "红包详细不存在"),
    RED_ENVELOP_RECEIVE_LIMIT_DIST(HttpServletResponse.SC_BAD_REQUEST, "非赚客不能领取红包"),
    RED_ENVELOP_STATUS_INVALID(HttpServletResponse.SC_BAD_REQUEST, "红包状态非法"),
    /**
     * Menu
     */
    MENU_EXISTING_LOWER_LEVEL_MENU(HttpServletResponse.SC_BAD_REQUEST, "当前菜单存在子菜单，不允许删除"),
    MENU_EXISTING_USING(HttpServletResponse.SC_BAD_REQUEST, "菜单已被使用，不允许删除"),
    MENU_NAME_EXIST(HttpServletResponse.SC_BAD_REQUEST, "菜单名称已存在"),
    /**
     * File
     */
    FILE_UPLOAD_FAIL(HttpServletResponse.SC_INTERNAL_SERVER_ERROR, "文件上传失败"),
    FILE_DOWNLOAD_FAIL(HttpServletResponse.SC_INTERNAL_SERVER_ERROR, "文件下载失败"),
    FILE_ILLEGAL_FILENAME(HttpServletResponse.SC_BAD_REQUEST, "文件名称非法，不允许下载"),
    /**
     * Job
     */
    JOB_NOT_FOUND(HttpServletResponse.SC_BAD_REQUEST, "未找到该定时任务"),
    /**
     * Config
     */
    CONFIG_KEY_EXIST(HttpServletResponse.SC_BAD_REQUEST, "参数键名已存在"),
    /**
     * Dict
     */
    DICT_TYPE_EXIST(HttpServletResponse.SC_BAD_REQUEST, "字典类型已存在"),
    /**
     * Post
     */
    POST_NAME_EXIST(HttpServletResponse.SC_BAD_REQUEST, "岗位名称已存在"),
    POST_CODE_EXIST(HttpServletResponse.SC_BAD_REQUEST, "岗位编码已存在"),
    /**
     * Data Access
     */
    DATE_ACCESS_FAIL(HttpServletResponse.SC_INTERNAL_SERVER_ERROR,"仓储服务出了点状况，抱歉"),
    ACCESS_BLOCK(HttpServletResponse.SC_INTERNAL_SERVER_ERROR,"当前服务限流"),
    /**
     * token
     */
    TOKEN_NOT_EXIST(HttpServletResponse.SC_BAD_REQUEST, "token不存在"),
    DUPLICATE_SUBMIT(HttpServletResponse.SC_BAD_REQUEST, "重复请求")

    ;

    //----------------------------------------------------分割线----------------------------------------------------
    /**
     * http状态码
     */
    private final int code;
    /**
     * 错误信息提示
     */
    private final String message;

    ErrorCodeEnum(int code, String message) {
        this.code = code;
        this.message = message;
    }

    /**
     * for example: JSON_FORMAT_ERROR
     *
     * @return json-format-error
     */
    public String getName() {
        return this.name().toLowerCase().replace('_', '-');
    }

    public static ErrorCodeEnum get(String errorCode) {
        for (ErrorCodeEnum errorCodeEnum : ErrorCodeEnum.values()) {
            if (errorCodeEnum.name().equalsIgnoreCase(errorCode)) {
                return errorCodeEnum;
            }
        }
        return null;
    }

    @Override
    public String toString() {
        return MessageFormat.format("{0}(code={1},message={2})", name().toLowerCase(), getCode(), getMessage());

    }
}
