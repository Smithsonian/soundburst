from panoptes_client import Project, Workflow, Panoptes, SubjectSet, Subject, Classification

##########################################
# Blue Raster test project (id=3159)
# emammal test project (id=2855)
##########################################

myID = 3159;
mySlug = 'sraby/blue-raster'
# myID = 2855;
# mySlug = 'sraby/emammal-species-identification';

##########################################
######### Get project title
##########################################
project = Project.find(id=myID)
print "Project title: " + project.title
print "#########"
print "Subject sets"
print "#########"

##########################################
######### Get subject sets
##########################################
for project in SubjectSet.where(project_id=myID):
    print project.display_name

print "#########"
print "Workflow name by slug"
print "#########"

classification = Classification.find('17063588')

classification = Classification.where()


classification.links.workflows = 2561

for property, value in vars(classification).iteritems():
    print property, ": ", value


##########################################
######### Get project workflows by slug
##########################################
project = Project.find(slug=mySlug)
for workflow in project.links.workflows:
    print "Workflow name: " + workflow.display_name

print "#########"
print "Workflow name by id"
print "#########"

##########################################
######### Get project workflows by id
##########################################
project = Project.find(id=myID)
for workflow in project.links.workflows:
    print "Workflow name: " + workflow.display_name

print "#########"
print "Create new project"
print "#########"
##########################################
######### Create new project
##########################################
# Panoptes.connect(username='sraby', password='BlueRaster2016!')
# p = Project()
# p.display_name='Test from python'
# p.description='This is created from the python client'
# p.primary_language='en'
# p.private=True
# p.save()

print "#########"
print "Create new subject set"
print "#########"
##########################################
######### Create new subject set and adding subject
##########################################
Panoptes.connect(username='sraby', password='BlueRaster2016!')
currProject = Project.find(id=3159)
subject_set = SubjectSet()
subject_set.links.project = currProject
subject_set.display_name = 'AdnanPython' # Change 
# Uncomment this line to precreate subject set without any images (comment code to add image after this line)
# Invisible in zooniverse.org until an image is added
#subject_set.save() 


subject = Subject()
subject.links.project = currProject
subject.add_location('/Users/tgirgin/Sites/zooniverse/api/Deer/2605s7i5_z.jpg')
subject.add_location('/Users/tgirgin/Sites/zooniverse/api/Deer/2605s7i7_z.jpg')
subject.add_location('/Users/tgirgin/Sites/zooniverse/api/Deer/2605s10i2_z.jpg')

subject.save()
subject_set.add(subject)

##########################################
######### Find subject set and add additional subject to it
##########################################

Panoptes.connect(username='sraby', password='BlueRaster2016!')
currProject = Project.find(id=myID)
for property, value in vars(project.links).iteritems():
    print property, ": ", value
print currProject.links.subject_sets

currSubjectSet = currProject.links.subject_sets[0]
print currSubjectSet.set_member_subjects_count
# currSubjectSet = SubjectSet.find(id=5988)
subject = Subject()
subject.links.project = currProject
subject.add_location('/Users/tgirgin/Sites/zooniverse/api/Deer/2605s7i5_z.jpg')
subject.add_location('/Users/tgirgin/Sites/zooniverse/api/Deer/2605s7i7_z.jpg')
subject.add_location('/Users/tgirgin/Sites/zooniverse/api/Deer/2605s10i2_z.jpg')

subject.save()
currSubjectSet.add(subject)

for property, value in vars(project.links).iteritems():
    print property, ": ", value

##########################################
######### Find Classification
##########################################

Panoptes.connect(username='sraby', password='BlueRaster2016!')
arrClassification = Classification.where(project_id=myID)
print arrClassification

for classificationss in arrClassification:
    print classificationss.metadata
