(define true #t)
(define false #f)
(define nil '())

class PolynomialArithmetic{
public:
    PolynomialArithmetic(const string tagIn="polynomial"):tagString(tagIn){
        put(makeList("gcd",makeList(this->getTag(),this->getTag())),
            makeLeaf(function<List(List,List)>
                     ([this](const List& x,const List& y)
                      {return(this->tag(this->gcd(x,y)));})));
        put(makeList("add",makeList(this->getTag(),this->getTag())),
            makeLeaf(function<List(List,List)>
                     ([this](const List& x,const List& y)
                      {return(drop(this->tag(this->addPolynomial(x,y))));})));
        put(makeList("sub",makeList(this->getTag(),this->getTag())),
            makeLeaf(function<List(List,List)>
                     ([this](const List& x,const List& y)
                      {return(drop(this->tag(this->subPolynomial(x,y))));})));
        put(makeList("mul",makeList(this->getTag(),this->getTag())),
            makeLeaf(function<List(List,List)>
                     ([this](const List& x,const List& y)
                      {return(drop(this->tag(this->mulPolynomial(x,y))));})));
        put(makeList("div",makeList(this->getTag(),this->getTag())),
            makeLeaf(function<List(List,List)>
                     ([this](const List& x,const List& y)
                      {return(drop(this->tag(this->divPolynomial(x,y))));})));
        put(makeList("constant?",makeList(this->getTag())),
            makeLeaf(function<List(List)>
                     ([this](const List& x)
                      {return(this->isConstant(x));})));
        put(makeList("=zero?",makeList(this->getTag())),
            makeLeaf(function<List(List)>
                     ([this](const List& x)
                      {return(this->isZero(x));})));
        put(makeList("negate",makeList(this->getTag())),
            makeLeaf(function<List(List)>
                     ([this](const List& x)
                      {return(this->tag(this->negate(x)));})));
        put(makeList("variable",makeList(this->getTag())),
            makeLeaf(function<List(List)>
                     ([this](const List& x)
                      {return(this->variable(x));})));
        put(makeList("leadingCoefficient",makeList(this->getTag())),
            makeLeaf(function<List(List)>
                     ([this](const List& x)
                      {return(this->leadingCoefficient(x));})));
        put(makeList("polynomialOrder",makeList(this->getTag())),
            makeLeaf(function<List(List)>
                     ([this](const List& x)
                      {return(this->polynomialOrder(x));})));
        put(makeList("expressionString",makeList(this->getTag())),
            makeLeaf(function<List(List)>
                     ([this](const List& x)
                      {return(this->expressionString(x));})));
        put(makeList("make",this->getTag()),
            makeLeaf(function<List(List,List)>
                     ([this](const List& var, const List& terms)
                      {return(this->tag
                              (this->makePolynomial(var,terms)));})));

    }
    virtual ~PolynomialArithmetic(void){};
   
    const List makePolynomial(const List& variable, const List& termList)const
    {return(makeList(variable,termList));}
   
    const List variable(const List& polynomial)const
    {return(car(polynomial));}

    const List termList(const List& polynomial)const
    {return(cadr(polynomial));}
   
    //same-variable?
    const bool isSameVariable(const List& v1,const List& v2)const
    {return(isVariable(v1) && isVariable(v2) && isEq(v1,v2));}

    //variable?
    const bool isVariable(const List& x)const
    {return(isSymbol(x));}


    const List gcd(const List& a, const List& b)const{
        if(makeLeaf(0)==b){return(a);}
        return(gcd(b,a%b));
    }

    const List addPolynomial
    (const List& p1, const List& p2)const
    {
        if(this->isSameVariable
           (this->variable(p1),this->variable(p2))){
            return(this->makePolynomial
                   (this->variable(p1),
                    this->addTerms(termList(p1),termList(p2))));
        }
        cerr<<"Polynomials not in same variable -- ADD-POLY "
            <<listString(p1)<<listString(p2)<<endl;
        exit(1);
        return(makeList());
    }

    const List addTerms(const List& L1,const List& L2)const
    {
        if(this->isEmptyTermList(L1)){return(L2);}
        else if(this->isEmptyTermList(L2)){return(L1);}

        const List t1(this->firstTerm(L1));
        const List t2(this->firstTerm(L2));
        if(this->order(t1)>this->order(t2)){
            return(this->adjoinTerm
                   (t1,this->addTerms(this->restTerms(L1),L2)));
        }else if(this->order(t1)<this->order(t2)){
            return(this->adjoinTerm
                   (t2,this->addTerms(L1,this->restTerms(L2))));
        }
        return(this->adjoinTerm
               (this->makeTerm
                (this->order(t1),
                 Generic::add(this->coefficient(t1),
                              this->coefficient(t2))),
                this->addTerms(this->restTerms(L1),
                               this->restTerms(L2))));
    }

    const List subPolynomial
    (const List&, const List&)const
    {return(makeList());}

    const List mulPolynomial
    (const List& p1, const List& p2)const
    {
        if(this->isSameVariable
           (this->variable(p1),this->variable(p2))){
            return(this->makePolynomial
                   (this->variable(p1),
                    this->mulTerms(termList(p1),termList(p2))));
        }
        cerr<<"Polynomials not in same variable -- MUL-POLY "
            <<listString(p1)<<listString(p2)<<endl;
        exit(1);
        return(makeList());
    }

    const List mulTerms(const List& L1, const List& L2)const
    {
        if(this->isEmptyTermList(L1))
            {return(this->theEmptyTermList());}
        return(this->addTerms
               (this->mulTermsByAllTerms(this->firstTerm(L1),L2),
                this->mulTerms(this->restTerms(L1),L2)));
    }

    const List mulTermsByAllTerms(const List& t1, const List& L)const
    {
        if(this->isEmptyTermList(L))
            {return(this->theEmptyTermList());}

        const List t2(this->firstTerm(L));
        return(this->adjoinTerm
               (this->makeTerm
                (this->order(t1)+this->order(t2),
                 Generic::mul(this->coefficient(t1),this->coefficient(t2))),
                this->mulTermsByAllTerms(t1,this->restTerms(L))));
    }

    const List divPolynomial
    (const List& x, const List& y)const
    {return(makeList());}

    const List adjoinTerm(const List& term, const List& termList)const
    {
        if(Generic::isZero(this->coefficient(term)))
            {return(termList);}
        return(cons(term,termList));
    }

    const List theEmptyTermList(void)const{return(makeList());}

    const List firstTerm(const List& termList)const
    {return(car(termList));}

    const List restTerms(const List& termList)const
    {return(cdr(termList));}
   
    const bool isEmptyTermList(const List& termList)const
    {return(isNull(termList));}

    const List makeTerm(const List& order,const List& coefficient)const
    {return(makeList(order,coefficient));}

    const List order(const List& term)const{return(car(term));}

    const List coefficient(const List& term)const{return(cadr(term));}

    const List isConstant(const List& x)const
    {
        return(makeLeaf(this->order
                        (this->firstTerm(this->termList(x)))==makeLeaf(0)));
    }
   
    const List isZero(const List& x)const
    {
        return(makeLeaf
               (isNull(termList(x))
                ||(this->isConstant(x)
                   && Generic::isZero(this->leadingCoefficient(x)))));
    }
   
    const List negate(const List& x)const
    {
        return(this->mulPolynomial
               (x,
                this->makePolynomial
                (this->variable(x),makeList(this->makeTerm(makeLeaf(0),Generic::makeNumber(-1))))));
    }

    const List leadingCoefficient(const List& x)const
    {
        return(this->coefficient(this->firstTerm(this->termList(x))));
    }

    const List polynomialOrder(const List& x)const
    {
        return(this->order(this->firstTerm(this->termList(x))));
    }


    const List expressionString(const List& x)const
    {
        string returnString("");
        if(this->isZero(x)!=makeLeaf(0)){return(makeLeaf(returnString));}

        const string coefString
            (Generic::expressionString(this->leadingCoefficient(x)));
        if(typeTag(this->leadingCoefficient(x))=="polynomial"
           ||typeTag(this->leadingCoefficient(x))=="complex"){
            if(this->polynomialOrder(x)!=makeLeaf(0)){
                returnString+="+("+coefString+")";
            }else{
                if(coefString.front()=='-'){
                    returnString+=coefString;
                }else{
                    returnString+="+"+coefString;
                }
            }
        }else if(coefString.front()=='-'){
            if(typeTag(this->leadingCoefficient(x))=="rational"){
                returnString+="-("
                    +Generic::expressionString
                    (Generic::negate(this->leadingCoefficient(x)))
                    +")";
            }else{
                if(Generic::isEqu(this->leadingCoefficient(x),Generic::makeNumber(-1))
                   && this->polynomialOrder(x)!=makeLeaf(0)){
                    returnString+="-";
                }else{
                    returnString+=coefString;
                }
            }
        }else{
            if(typeTag(this->leadingCoefficient(x))=="rational"){
                returnString+="+("+coefString+")";
            }else{
                if(Generic::isEqu(this->leadingCoefficient(x),Generic::makeNumber(1))
                   && this->polynomialOrder(x)!=makeLeaf(0)){
                    returnString+="+";
                }else{
                    returnString+="+"+coefString;
                }
            }
        }

        if(this->polynomialOrder(x)!=makeLeaf(0)){
            returnString+=listString(this->variable(x));
            if(this->polynomialOrder(x)!=makeLeaf(1)){
                returnString+="^"+listString(this->polynomialOrder(x));
            }
        }

        return(makeLeaf
               (returnString
                +this->expressionString
                (this->makePolynomial(this->variable(x),cdr(this->termList(x))))->getItem()));
    }
   
    const TagType getTag(void)const{return(this->tagString);}
   
    virtual const List tag(const List& x)const
    {return(attachTag(this->getTag(),x));}
   
private:
    const TagType tagString;
};

PolynomialArithmetic* _polynomialPackage(nullptr);

void installPolynomialPackage(void){
    _polynomialPackage=new PolynomialArithmetic();
}

void uninstallPolynomialPackage(void){
    if(nullptr!=_polynomialPackage) delete _polynomialPackage;
}
//---------abstraction barrier---------


int main(int argc, char** argv)
{
    installNumberPackage();
    installRationalPackage();
    installRealPackage();
    installComplexPackage();
    installPolynomialPackage();
    installCoercion();

    using namespace Generic;

    const List p1(makePolynomial
                  ("x",makeList
                   (makeList(2,makeNumber(3)),
                    makeList(1,makeComplexFromRealImag(2,3)),
                    makeList(0,makeNumber(7)))));
    const List p2(makePolynomial
                  ("x",makeList
                   (makeList(4,makeNumber(1)),
                    makeList(2,makeRational(2,3)),
                    makeList(0,makeComplexFromRealImag(5,3))
                    )));
   
    cout<<"p1 = "<<expressionString(p1)<<endl;
    cout<<"p2 = "<<expressionString(p2)<<endl;
    cout<<"(add p1 p2) = "<<expressionString(add(p1,p2))<<endl;
    cout<<"(mul p1 p2) = "<<expressionString(mul(p1,p2))<<endl;

    const List py1(makePolynomial
                   ("y",makeList
                    (makeList(1,makeNumber(1)),
                     makeList(0,makeNumber(1)))));
    const List py2(makePolynomial
                   ("y",makeList
                    (makeList(2,makeNumber(1)),
                     makeList(0,makeNumber(1)))));
    const List py3(makePolynomial
                   ("y",makeList
                    (makeList(1,makeNumber(1)),
                     makeList(0,makeNumber(-1)))));
    const List pxy1(makePolynomial
                    ("x",makeList
                     (makeList(2,py1),
                      makeList(1,py2),
                      makeList(0,py3))));
    cout<<"pxy1 = "<<expressionString(pxy1)<<endl;
    const List py4(makePolynomial
                   ("y",makeList
                    (makeList(1,makeNumber(1)),
                     makeList(0,makeNumber(-2)))));
    const List py5(makePolynomial
                   ("y",makeList
                    (makeList(3,makeNumber(1)),
                     makeList(0,makeNumber(7)))));
    const List pxy2(makePolynomial
                    ("x",makeList
                     (makeList(1,py4),
                      makeList(0,py5))));
    cout<<"pxy2 = "<<expressionString(pxy2)<<endl;
    cout<<"(mul pxy1 pxy2) = "<<expressionString(mul(pxy1,pxy2))<<endl;

    cout<<endl<<"Excersize 2.87:"<<endl;
    const List p3(makePolynomial("x",makeList(makeList(0,makeNumber(0)))));
    cout<<"p3 = "<<expressionString(drop(p3))<<endl;
    cout<<"(=zero? p3) = "<<isZero(p3)<<endl;
    cout<<"(mul p1 p3) = "<<expressionString(mul(p1,p3))<<endl;

    cout<<endl<<"Excersize 2.88:"<<endl;
    const List p4(Generic::negate(p2));
    cout<<"p4 = (negate p2) = "<<expressionString(p4)<<endl;
    cout<<"(add p2 p4) = "<<expressionString(add(p2,p4))<<endl;
    cout<<"(sub p1 p2) = "<<expressionString(sub(p1,p2))<<endl;

    uninstallNumberPackage();
    uninstallRationalPackage();
    uninstallRealPackage();
    uninstallComplexPackage();
    uninstallPolynomialPackage();
    return(0);
}