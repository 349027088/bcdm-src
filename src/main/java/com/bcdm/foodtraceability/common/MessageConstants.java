package com.bcdm.foodtraceability.common;

/**
 * <p>
 * 页面返回信息常量配置类
 * </p>
 *
 * @author 王
 * @since 2022-01-16
 */
public class MessageConstants {

    /** 服务器异常 */
    public static final String SEVER_ERROR = "服务器异常";

    /** 空指针异常 */
    public static final String NOT_POINT_ERROR = "空指针异常";

    /** 运行时异常 */
    public static final String RUNTIME_ERROR = "运行时异常";

    /** 登录成功 */
    public static final String LOGIN_SUCCESS = "登录成功";

    /** 账号密码错误 */
    public static final String LOGIN_FAIL = "账号密码错误";

    /** 注册成功 */
    public static final String REGISTER_SUCCESS = "注册成功";

    /** 当前账号已被使用 */
    public static final String REGISTER_FAIL = "当前账号已被使用";

    /** 密码修改成功 */
    public static final String MODIFY_PASSWORD_SUCCESS = "密码修改成功";

    /** 密码修改失败 */
    public static final String MODIFY_PASSWORD_FAIL = "密码修改失败";

    /** 修改用户信息失败 */
    public static final String MODIFY_USERINFO_FAIL = "修改用户信息失败";

    /** 用户锁定失败 */
    public static final String LOCK_USER_FAIL = "用户锁定失败";

    /** 用户解锁失败 */
    public static final String UNLOCK_USER_FAIL = "用户解锁失败";

    /** 没有获得用户绑定的公司信息 */
    public static final String USER_GET_COMPANY_INFO_FAIL = "没有获得用户绑定的公司信息";

    /** 员工信息获取失败 */
    public static final String COMPANY_GET_USER_INFO_FAIL = "员工信息获取失败";
}
