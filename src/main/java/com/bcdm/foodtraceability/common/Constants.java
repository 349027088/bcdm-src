package com.bcdm.foodtraceability.common;

/**
 * <p>
 * 常量配置类
 * </p>
 *
 * @author 王
 * @since 2022-01-09
 */
public class Constants {

    /** 性别男  */
    public static final Integer USER_SEX_MAN = 1;

    /** 性别女  */
    public static final Integer USER_SEX_WOMEN = 0;

    /** 普通管理员级别  */
    public static final Integer MANAGEMENT_NORMAL_LEVEL = 1;

    /** 管理员全级别  */
    public static final Integer MANAGEMENT_HIGH_LEVEL = 2;

    /** 查询结果为0  */
    public static final Integer SELECT_ZERO = 0;

    /** 单个用户公司创建上限 */
    public static final Integer COMPANY_MAX = 3;

    /** . */
    public static final String CUT_POINT = ".";

    /** 系统自动创建管理员编号 */
    public static final Integer DEFAULT_MANAGEMENT_ID = 0;

    /** 默认存入金额 */
    public static final Integer DEFAULT_MONEY_AMT = 0;

    /** 默认启动时限 */
    public static final Integer DEFAULT_USAGE_TIME = 31;


    // --------------------------------管理员状态--------------------------------

    /** 解锁状态  */
    public static final Integer MANAGEMENT_STATUS_UNLOCK = 0;
    // --------------------------------用户状态--------------------------------

    /** 解锁状态  */
    public static final Integer USER_STATUS_UNLOCK = 0;

    /** 加锁状态  */
    public static final Integer USER_STATUS_LOCK = 1;

    // --------------------------------企业状态--------------------------------

    /** 企业状态：未提供服务 */
    public static final Integer COMPANY_STATUS_OUT_OF_SERVICE = 0;

    /** 企业状态：正在提供服务 */
    public static final Integer COMPANY_STATUS_ON_SERVICE = 1;

    /** 企业状态：企业正在被锁定 */
    public static final Integer COMPANY_STATUS_IS_LOCK = 2;

    /** 企业状态：企业关闭 */
    public static final Integer COMPANY_STATUS_CLOSE = 3;

    // --------------------------------企业状态--------------------------------

    /** 企业状态：未提供服务 */
    public static final Integer COMPANY_LEVEL_NORMAL = 0;

    /** 企业状态：未提供服务 */
    public static final Integer COMPANY_LEVEL_HIGH_ = 1;

    // --------------------------------关联状态--------------------------------

    /** 企业老板 */
    public static final Integer COMPANY_USER_0 = 0;

    /** 操作员 */
    public static final Integer COMPANY_USER_1 = 1;

    /** 普通员工 */
    public static final Integer COMPANY_USER_2 = 2;

    /** 员工登陆中 */
    public static final Integer COMPANY_USER_3 = 3;





}
