package com.bcdm.foodtraceability.controller;


import com.baomidou.mybatisplus.extension.api.R;
import com.bcdm.foodtraceability.entity.Company;
import com.bcdm.foodtraceability.entity.ReturnItem;
import com.bcdm.foodtraceability.entity.User;
import com.bcdm.foodtraceability.service.CompanyService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import java.util.List;

import static com.bcdm.foodtraceability.common.HttpConstants.HTTP_RETURN_SUCCESS;

/**
 * <p>
 * 前端控制器
 * </p>
 *
 * @author 王
 * @since 2022-01-13
 */
@RestController
@RequestMapping("/company")
public class CompanyController {

    @Autowired
    private CompanyService companyService;

    /**
     * 企业登录Controller
     * @param user 登录企业的用户
     * @param company 新加入的企业信息
     * @return 创建成功的企业信息
     * @throws Exception 创建失败
     */
    @PostMapping("/register")
    @CrossOrigin
    public ReturnItem<Company> register(@RequestBody User user, Company company) throws Exception {
        ReturnItem<Company> returnItem = new ReturnItem<>();
        returnItem.setT(companyService.register(user, company));
        returnItem.setHttpStatus(HTTP_RETURN_SUCCESS);
        returnItem.setHttpMessage("企业登录成功");
        return returnItem;
    }

    /**
     * 企业信息修改Controller
     * @param user 修改信息的用户
     * @param company 需要需改的公司信息
     * @return 修改成功的公司信息
     * @throws Exception 修改信息失败
     */
    @PostMapping("/modify")
    @CrossOrigin
    public ReturnItem<Company> modify(@RequestBody User user, Company company) throws Exception {
        ReturnItem<Company> returnItem = new ReturnItem<>();
        returnItem.setT(companyService.modify(user, company));
        returnItem.setHttpStatus(HTTP_RETURN_SUCCESS);
        returnItem.setHttpMessage("企业信息修改成功");
        return returnItem;
    }

    /**
     * 获得公司信息
     * @param user 获取公司信息的用户
     * @return 该用户所属公司信息
     * @throws Exception 查询公司信息失败
     */
    @GetMapping("/getCompanyInfo")
    @CrossOrigin
    public ReturnItem<List<Company>> getCompanyInfo(@RequestBody User user) throws Exception {
        ReturnItem<List<Company>> returnItem = new ReturnItem<>();
        returnItem.setT(companyService.getCompanyInfo(user));
        returnItem.setHttpStatus(HTTP_RETURN_SUCCESS);
        returnItem.setHttpMessage("企业信息获取成功");
        return returnItem;
    }

    @PostMapping("createCompanyIcon")
    @CrossOrigin
    public ReturnItem<Company> createCompanyIcon(){
        ReturnItem<Company> returnItem = new ReturnItem<>();
        returnItem.setHttpStatus(HTTP_RETURN_SUCCESS);
        returnItem.setHttpMessage("企业图片上传成功");
        return returnItem;
    }

    @PostMapping("modifyCompanyIcon")
    @CrossOrigin
    public ReturnItem<Company> modifyCompanyIcon(){
        ReturnItem<Company> returnItem = new ReturnItem<>();
        returnItem.setHttpStatus(HTTP_RETURN_SUCCESS);
        returnItem.setHttpMessage("企业图片修改成功");
        return returnItem;
    }


}

