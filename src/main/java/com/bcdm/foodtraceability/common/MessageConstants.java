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
    public static final String LOGIN_FAIL = "当前账号不存在或者输入的密码有误";

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
    public static final String USER_GET_COMPANY_INFO_FAIL1 = "没有获得用户绑定的公司信息";

    /** 公司正在审核中 */
    public static final String USER_GET_COMPANY_INFO_FAIL2 = "您申请的公司正在审核中";

    /** 公司锁定 */
    public static final String USER_GET_COMPANY_INFO_FAIL3 = "您的公司已经被锁定，请联系官方咨询详细信息";

    /** 当前企业已关闭 */
    public static final String USER_GET_COMPANY_INFO_FAIL4 = "当前企业已关闭";

    /** 没有对应的员工信息 */
    public static final String COMPANY_GET_USER_INFO_FAIL = "没有对应的员工信息";

    /** 企业注册成功 */
    public static final String CREATE_COMPANY_SUCCESS = "企业注册已经成功，请耐心等待管理员审核";

    /** 企业信息修改成功 */
    public static final String MODIFY_COMPANY_INFO_SUCCESS = "企业信息修改成功";

    /** 上传新的营业执照成功 */
    public static final String UPLOAD_BUSINESS_LICENSE_INFO_SUCCESS = "上传新的营业执照成功，请等待管理员得审核";

    /** 上传新的经营资格执照成功 */
    public static final String UPLOAD_HEALTH_PERMIT_INFO_SUCCESS = "上传新的经营资格执照成功，请等待管理员得审核";

    /** 员工信息获取成功 */
    public static final String COMPANY_USER_GET_SUCCESS = "员工信息获取成功";

    /** 企业图片上传成功 */
    public static final String ICON_UPLOAD_SUCCESS = "企业图片上传成功";

    /** 修改员工职位成功 */
    public static final String MODIFY_USER_TO_COMPANY_SUCCESS = "修改员工职位成功";

    /** 修改通知最新时间成功 */
    public static final String MODIFY_NOTICE_CHECK_SUCCESS = "修改通知最新时间成功";

    /** 登录到企业成功 */
    public static final String CREATE_USER_TO_COMPANY_SUCCESS = "登录到企业成功";

    /** 添加企业信息失败 */
    public static final String CREATE_COMPANY_FAIL = "添加企业信息失败";

    /** 获取企业信息失败 */
    public static final String GET_COMPANY_LIST_FAIL = "获取企业信息失败";

    /** 企业信息获取失败 */
    public static final String GET_COMPANY_INFO_FAIL = "当前企业服务已到期，无法查询到该公司的信息";

    /** 企业信息获取成功 */
    public static final String GET_COMPANY_INFO_SUCCESS = "企业信息获取成功";

    /** 修改企业信息失败 */
    public static final String MODIFY_COMPANY_FAIL = "修改企业信息失败";

    /** 图片类型不符合规范 */
    public static final String ICON_TYPE_FORMAT_FAIL = "图片类型不符合规范";

    /** 图片上传失败 */
    public static final String ICON_UPLOAD_FAIL = "图片上传失败";

    /** 图片超过大小限制 */
    public static final String ICON_SIZE_FAIL = "图片超过大小限制";

    /** 修改企业授权信息失败*/
    public static final String MODIFY_EMPOWER_FAIL = "修改企业授权信息失败";

    /**企业授权失败*/
    public static final String ENTERPRISE_AUTHORIZATION_FAILED = "企业授权失败";

    /** 创建授权信息失败 */
    public static final String CREATE_EMPOWER_FAIL = "创建授权信息失败";

    /** 创建关联信息失败 */
    public static final String CREATE_JURISDICTION_FAIL = "创建关联信息失败";

    /** 修改权限信息失败 */
    public static final String MODIFY_JURISDICTION_FAIL = "修改职位失败";

    /** 更新读取通知时间失败 */
    public static final String MODIFY_NOTICE_CHECK_FAIL = "更新读取通知时间失败";

    /** 修改权限级别不足 */
    public static final String MODIFY_JURISDICTION_LEVEL_FAIL = "修改权限级别不足";

    /** 修改权限信息失败 */
    public static final String DELETE_JURISDICTION_FAIL = "删除员工失败";

    /** 删除员工级别不足 */
    public static final String DELETE_JURISDICTION_LEVEL_FAIL = "删除员工级别不足";

    /** 创建供应商失败 */
    public static final String CREATE_SUPPLIER_FAIL = "创建供应商失败";

    /** 修改供应商信息失败 */
    public static final String MODIFY_SUPPLIER_FAIL = "修改供应商信息失败";

    /** 查询授权信息失败 */
    public static final String SELECT_EMPOWER_FAIL = "查询授权信息失败";

    /** 查询供应商信息失败 */
    public static final String SELECT_SUPPLIER_INFO_FAIL = "查询供应商信息失败";

    /** 删除供应商信息失败 */
    public static final String DELETE_SUPPLIER_INFO_FAIL = "删除供应商信息失败";

    /** 正在等待企业承认您的申请 */
    public static final String USER_ADMIT_FAIL = "正在等待企业承认您的申请";

    /** 添加商品种类信息失败 */
    public static final String ADD_GOODS_TYPE_FAIL = "添加商品种类信息失败";

    /** 添加生产厂商信息失败 */
    public static final String ADD_MANUFACTURER_FAIL = "添加生产厂商信息失败";

    /** 删除商品种类信息失败 */
    public static final String DELETE_GOODS_TYPE_FAIL = "删除商品种类信息失败";

    /** 删除生产厂商信息失败 */
    public static final String DELETE_MANUFACTURER_FAIL = "删除生产厂商信息失败";

    /** 获取商品种类信息失败或者查出0条 */
    public static final String SELECT_GOODS_TYPE_INFO_FAIL = "获取商品种类信息失败或当前没有设置商品种类信息";

    /** 获取生产厂商信息失败或者查出0条 */
    public static final String SELECT_MANUFACTURER_INFO_FAIL = "获取生产厂商信息失败或当前没有设置生产厂商信息";

    /** 添加供应商信息成功 */
    public static final String ADD_SUPPLIER_INFO_SUCCESS = "添加供应商信息成功";

    /** 获取供应商信息成功 */
    public static final String SELECT_SUPPLIER_INFO_SUCCESS = "获取供应商信息成功";

    /** 获取商品信息成功 */
    public static final String SELECT_GOODS_INFO_SUCCESS = "获取商品信息成功";

    /** 添加商品信息成功 */
    public static final String ADD_GOODS_INFO_SUCCESS = "添加商品信息成功";

    /** 修改商品信息成功 */
    public static final String MODIFY_GOODS_INFO_SUCCESS = "修改商品信息成功";

    /** 删除商品信息成功 */
    public static final String DELETE_GOODS_INFO_SUCCESS = "删除商品信息成功";

    /** 修改供应商信息成功 */
    public static final String MODIFY_SUPPLIER_INFO_SUCCESS = "修改供应商信息成功";

    /** 删除供应商信息成功 */
    public static final String DELETE_SUPPLIER_INFO_SUCCESS = "删除供应商信息成功";

    /** 添加生产厂商信息成功 */
    public static final String ADD_MANUFACTURER_SUCCESS = "添加生产厂商信息成功";

    /** 获取生产厂商信息成功 */
    public static final String SELECT_MANUFACTURER_SUCCESS = "获取生产厂商信息成功";

    /** 修改生产厂商信息成功 */
    public static final String MODIFY_MANUFACTURER_SUCCESS = "修改生产厂商信息成功";

    /** 删除生产厂商信息成功 */
    public static final String DELETE_MANUFACTURER_SUCCESS = "删除生产厂商信息成功";

    /** 增加商品种类信息成功 */
    public static final String ADD_GOODS_TYPE_SUCCESS = "增加商品种类信息成功";

    /** 获取商品种类信息成功 */
    public static final String SELECT_GOODS_TYPE_INFO_SUCCESS = "获取商品种类信息成功";

    /** 修改商品种类信息成功 */
    public static final String MODIFY_GOODS_TYPE_SUCCESS = "修改商品种类信息成功";

    /** 删除商品种类信息成功 */
    public static final String DELETE_GOODS_TYPE_SUCCESS = "删除商品种类信息成功";

    /** 当前修改种类名称重复或已被修改 */
    public static final String FIND_GOODS_TYPE_NAME_BY_COMPANY_FAIL1 = "当前修改种类名称重复或已被修改";

    /** 当前修改生产厂商名称重复或已被修改 */
    public static final String FIND_MANUFACTURER_NAME_BY_COMPANY_FAIL1 = "当前修改生产厂商名称重复或已被修改";

    /** 当前修改供应商名称重复或已被修改 */
    public static final String FIND_SUPPLIER_NAME_BY_COMPANY_FAIL1 = "当前修改供应商名称重复或已被修改";

    /** 当前生产厂商已经存在 */
    public static final String FIND_MANUFACTURER_NAME_BY_COMPANY_FAIL2 = "当前生产厂商已经存在";

    /** 当前供应商名称已经存在 */
    public static final String FIND_SUPPLIER_NAME_BY_COMPANY_FAIL2 = "当前供应商名称已经存在";

    /** 当前种类名称已经存在 */
    public static final String FIND_GOODS_TYPE_NAME_BY_COMPANY_FAIL2 = "当前种类名称已经存在";

    /** 当前生产厂商不存在 */
    public static final String FIND_MANUFACTURER_NAME_BY_COMPANY_FAIL3 = "当前生产厂商不存在";

    /** 当前供应商不存在 */
    public static final String FIND_SUPPLIER_NAME_BY_COMPANY_FAIL3 = "当前供应商不存在";

    /** 当前种类不存在 */
    public static final String FIND_GOODS_TYPE_NAME_BY_COMPANY_FAIL3 = "当前种类不存在";

    /** 修改种类名称失败 */
    public static final String MODIFY_GOODS_TYPE_FAIL = "修改种类名称失败";

    /** 修改生产厂商失败 */
    public static final String MODIFY_MANUFACTURER_FAIL = "修改生产厂商失败";

    /** 获取指定的商品信息失败 */
    public static final String SELECT_GOODS_INFO_FAIL = "当前没有指定的商品信息";

    /** 该供应商里这个商品名称已经存在 */
    public static final String FIND_GOODS_NAME_BY_COMPANY_FAIL1 = "该供应商里这个商品名称已经存在";

    /** 当前商品已经存在 */
    public static final String FIND_GOODS_NAME_BY_COMPANY_FAIL2 = "当前商品已经存在";

    /** 当前商品不存在 */
    public static final String FIND_GOODS_NAME_BY_COMPANY_FAIL3 = "当前商品不存在";

    /** 增加商品信息失败 */
    public static final String ADD_GOODS_INFO_FAIL = "增加商品信息失败";

    /** 修改商品信息失败 */
    public static final String MODIFY_GOODS_INFO_FAIL = "修改商品信息失败";

    /** 删除商品信息失败 */
    public static final String DELETE_GOODS_INFO_FAIL = "删除商品信息失败或者当前商品信息不存在";

    /** 当前商品的关联信息获取失败或已删除 */
    public static final String CHECK_INFO_FAIL = "当前商品的关联信息获取失败或已删除，请刷新后重试";

    /** 创建商品二维码失败 */
    public static final String CREATE_BARCODE_FAIL = "创建商品二维码失败";

    /** 创建新的消息通知失败 */
    public static final String CREATE_NOTICE_FAIL = "创建新的消息通知失败";

    /** 创建商品二维码成功 */
    public static final String CREATE_BARCODE_SUCCESS = "创建商品二维码成功";

    /** 发布通知信息成功 */
    public static final String CREATE_NOTICE_SUCCESS = "发布新的通知成功";

    /** 修改通知信息成功 */
    public static final String MODIFY_NOTICE_SUCCESS = "修改通知信息成功";

    /** 查询通知信息成功 */
    public static final String SELECT_NOTICE_SUCCESS = "查询通知信息成功";

    /** 删除通知信息成功 */
    public static final String DELETE_NOTICE_SUCCESS = "删除通知信息成功";

    /** 获取城市信息成功 */
    public static final String GET_CITY_LIST_SUCCESS = "获取城市信息成功";

    /** 获取城市信息失败 */
    public static final String GET_CITY_LIST_FAIL = "获取城市信息失败";

    /** 获取通知信息失败 */
    public static final String GET_NOTICE_FAIL = "获取通知信息失败";

    /** 修改通知信息失败 */
    public static final String MODIFY_NOTICE_FAIL = "修改通知信息失败";

    /** 删除通知信息失败 */
    public static final String DELETE_NOTICE_FAIL = "删除通知信息失败";

    /** 非管理员 */
    public static final String ERROR_FOR_GET_MANAGER = "警告！当前修改人员非本社管理人员";

    /** 修改企业营业执照信息失败 */
    public static final String MODIFY_COMPANY_BUSINESS_LICENSE_FAIL = "修改企业营业执照信息失败";

    /** 修改企业经营执照信息失败 */
    public static final String MODIFY_COMPANY_HEALTH_PERMIT_FAIL = "修改企业经营执照信息失败";

    /** 修改信息已存在 */
    public static final String MODIFY_COMPANY_IS_FAIL = "当前存在正在修改的经营信息，请耐心等待审核";

    /** 上传的文件不是正确的图片格式 */
    public static final String UPLOAD_FILE_IS_NOT_ICON = "上传的文件不是正确的图片格式";
}
